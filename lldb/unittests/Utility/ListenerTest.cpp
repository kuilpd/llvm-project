//===-- ListenerTest.cpp --------------------------------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#include "gtest/gtest.h"

#include "lldb/Utility/Broadcaster.h"
#include "lldb/Utility/Event.h"
#include "lldb/Utility/Listener.h"
#include <future>
#include <thread>

using namespace lldb;
using namespace lldb_private;

TEST(ListenerTest, GetEventImmediate) {
  EventSP event_sp;
  Broadcaster broadcaster(nullptr, "test-broadcaster");

  // Create a listener, sign it up, make sure it receives an event.
  ListenerSP listener_sp = Listener::MakeListener("test-listener");
  const uint32_t event_mask = 1;
  ASSERT_EQ(event_mask,
            listener_sp->StartListeningForEvents(&broadcaster, event_mask));

  const std::chrono::seconds timeout(0);
  // Without any events sent, these should return false.
  EXPECT_FALSE(listener_sp->GetEvent(event_sp, timeout));
  EXPECT_FALSE(listener_sp->GetEventForBroadcaster(nullptr, event_sp, timeout));
  EXPECT_FALSE(
      listener_sp->GetEventForBroadcaster(&broadcaster, event_sp, timeout));
  EXPECT_FALSE(listener_sp->GetEventForBroadcasterWithType(
      &broadcaster, event_mask, event_sp, timeout));

  // Now send events and make sure they get it.
  broadcaster.BroadcastEvent(event_mask, nullptr);
  EXPECT_TRUE(listener_sp->GetEvent(event_sp, timeout));

  broadcaster.BroadcastEvent(event_mask, nullptr);
  EXPECT_TRUE(listener_sp->GetEventForBroadcaster(nullptr, event_sp, timeout));

  broadcaster.BroadcastEvent(event_mask, nullptr);
  EXPECT_TRUE(
      listener_sp->GetEventForBroadcaster(&broadcaster, event_sp, timeout));

  broadcaster.BroadcastEvent(event_mask, nullptr);
  EXPECT_FALSE(listener_sp->GetEventForBroadcasterWithType(
      &broadcaster, event_mask * 2, event_sp, timeout));
  EXPECT_TRUE(listener_sp->GetEventForBroadcasterWithType(
      &broadcaster, event_mask, event_sp, timeout));
}

TEST(ListenerTest, GetEventWait) {
  EventSP event_sp;
  Broadcaster broadcaster(nullptr, "test-broadcaster");

  // Create a listener, sign it up, make sure it receives an event.
  ListenerSP listener_sp = Listener::MakeListener("test-listener");
  const uint32_t event_mask = 1;
  ASSERT_EQ(event_mask,
            listener_sp->StartListeningForEvents(&broadcaster, event_mask));

  // Without any events sent, these should make a short wait and return false.
  std::chrono::microseconds timeout(10);
  EXPECT_FALSE(listener_sp->GetEvent(event_sp, timeout));
  EXPECT_FALSE(listener_sp->GetEventForBroadcaster(nullptr, event_sp, timeout));
  EXPECT_FALSE(
      listener_sp->GetEventForBroadcaster(&broadcaster, event_sp, timeout));
  EXPECT_FALSE(listener_sp->GetEventForBroadcasterWithType(
      &broadcaster, event_mask, event_sp, timeout));

  // Now send events and make sure they get it.
  broadcaster.BroadcastEvent(event_mask, nullptr);
  EXPECT_TRUE(listener_sp->GetEvent(event_sp, timeout));

  broadcaster.BroadcastEvent(event_mask, nullptr);
  EXPECT_TRUE(listener_sp->GetEventForBroadcaster(nullptr, event_sp, timeout));

  broadcaster.BroadcastEvent(event_mask, nullptr);
  EXPECT_TRUE(
      listener_sp->GetEventForBroadcaster(&broadcaster, event_sp, timeout));

  broadcaster.BroadcastEvent(event_mask, nullptr);
  EXPECT_FALSE(listener_sp->GetEventForBroadcasterWithType(
      &broadcaster, event_mask * 2, event_sp, timeout));
  EXPECT_TRUE(listener_sp->GetEventForBroadcasterWithType(
      &broadcaster, event_mask, event_sp, timeout));

  auto delayed_broadcast = [&] {
    std::this_thread::sleep_for(std::chrono::milliseconds(10));
    broadcaster.BroadcastEvent(event_mask, nullptr);
  };

  // These should do an infinite wait at return the event our asynchronous
  // broadcast sends.
  std::future<void> async_broadcast =
      std::async(std::launch::async, delayed_broadcast);
  EXPECT_TRUE(listener_sp->GetEvent(event_sp, std::nullopt));
  async_broadcast.get();

  async_broadcast = std::async(std::launch::async, delayed_broadcast);
  EXPECT_TRUE(listener_sp->GetEventForBroadcaster(&broadcaster, event_sp,
                                                  std::nullopt));
  async_broadcast.get();

  async_broadcast = std::async(std::launch::async, delayed_broadcast);
  EXPECT_TRUE(listener_sp->GetEventForBroadcasterWithType(
      &broadcaster, event_mask, event_sp, std::nullopt));
  async_broadcast.get();
}

TEST(ListenerTest, StartStopListeningForEventSpec) {
  constexpr uint32_t event_mask = 1;
  static constexpr llvm::StringLiteral broadcaster_class = "broadcaster-class";

  class TestBroadcaster : public Broadcaster {
    using Broadcaster::Broadcaster;
    llvm::StringRef GetBroadcasterClass() const override {
      return broadcaster_class;
    }
  };

  BroadcasterManagerSP manager_sp =
      BroadcasterManager::MakeBroadcasterManager();
  ListenerSP listener_sp = Listener::MakeListener("test-listener");

  // Create two broadcasters, one while we're waiting for new broadcasters, and
  // one when we're not.
  ASSERT_EQ(listener_sp->StartListeningForEventSpec(
                manager_sp, BroadcastEventSpec(broadcaster_class, event_mask)),
            event_mask);
  TestBroadcaster broadcaster1(manager_sp, "test-broadcaster-1");
  broadcaster1.CheckInWithManager();
  ASSERT_TRUE(listener_sp->StopListeningForEventSpec(
      manager_sp, BroadcastEventSpec(broadcaster_class, event_mask)));
  TestBroadcaster broadcaster2(manager_sp, "test-broadcaster-2");
  broadcaster2.CheckInWithManager();

  // Use both broadcasters to send an event.
  for (auto *b : {&broadcaster1, &broadcaster2})
    b->BroadcastEvent(event_mask, nullptr);

  // Use should only get the event from the first one.
  EventSP event_sp;
  ASSERT_TRUE(listener_sp->GetEvent(event_sp, std::chrono::seconds(0)));
  ASSERT_EQ(event_sp->GetBroadcaster(), &broadcaster1);
  ASSERT_FALSE(listener_sp->GetEvent(event_sp, std::chrono::seconds(0)));
}

TEST(ListenerTest, MoveEventsOnHijackAndRestore) {
  Broadcaster broadcaster(nullptr, "test-broadcaster");
  const uint32_t event_mask = 1;
  EventSP event_sp;

  // Create the original listener and start listening.
  ListenerSP original_listener = Listener::MakeListener("original-listener");
  ASSERT_EQ(event_mask, original_listener->StartListeningForEvents(&broadcaster,
                                                                   event_mask));
  broadcaster.SetPrimaryListener(original_listener);

  // Queue two events to original listener, but do not consume them yet.
  broadcaster.BroadcastEvent(event_mask, nullptr);
  broadcaster.BroadcastEvent(event_mask, nullptr);

  // Hijack.
  ListenerSP hijack_listener = Listener::MakeListener("hijack-listener");
  broadcaster.HijackBroadcaster(hijack_listener, event_mask);

  // The events should have been moved to the hijack listener.
  EXPECT_FALSE(original_listener->GetEvent(event_sp, std::chrono::seconds(0)));
  EXPECT_TRUE(hijack_listener->GetEvent(event_sp, std::chrono::seconds(0)));
  EXPECT_TRUE(hijack_listener->GetEvent(event_sp, std::chrono::seconds(0)));

  // Queue two events while hijacked, but do not consume them yet.
  broadcaster.BroadcastEvent(event_mask, nullptr);
  broadcaster.BroadcastEvent(event_mask, nullptr);

  // Restore the original listener.
  broadcaster.RestoreBroadcaster();

  // The events queued while hijacked should have been moved back to the
  // original listener.
  EXPECT_FALSE(hijack_listener->GetEvent(event_sp, std::chrono::seconds(0)));
  EXPECT_TRUE(original_listener->GetEvent(event_sp, std::chrono::seconds(0)));
  EXPECT_TRUE(original_listener->GetEvent(event_sp, std::chrono::seconds(0)));
}

TEST(ListenerTest, MoveEventsBetweenHijackers) {
  Broadcaster broadcaster(nullptr, "test-broadcaster");
  const uint32_t event_mask = 1;
  EventSP event_sp;

  // Create the original listener and start listening.
  ListenerSP original_listener = Listener::MakeListener("original-listener");
  ASSERT_EQ(event_mask, original_listener->StartListeningForEvents(&broadcaster,
                                                                   event_mask));
  broadcaster.SetPrimaryListener(original_listener);

  // First hijack.
  ListenerSP hijack_listener1 = Listener::MakeListener("hijack-listener1");
  broadcaster.HijackBroadcaster(hijack_listener1, event_mask);

  // Queue two events while hijacked, but do not consume
  // them yet.
  broadcaster.BroadcastEvent(event_mask, nullptr);
  broadcaster.BroadcastEvent(event_mask, nullptr);

  // Second hijack.
  ListenerSP hijack_listener2 = Listener::MakeListener("hijack-listener2");
  broadcaster.HijackBroadcaster(hijack_listener2, event_mask);

  // The second hijacker should have the events now.
  EXPECT_FALSE(hijack_listener1->GetEvent(event_sp, std::chrono::seconds(0)));
  EXPECT_TRUE(hijack_listener2->GetEvent(event_sp, std::chrono::seconds(0)));
  EXPECT_TRUE(hijack_listener2->GetEvent(event_sp, std::chrono::seconds(0)));

  // Queue two events while hijacked with second hijacker, but do not consume
  // them yet.
  broadcaster.BroadcastEvent(event_mask, nullptr);
  broadcaster.BroadcastEvent(event_mask, nullptr);

  // Restore the previous hijacker.
  broadcaster.RestoreBroadcaster();

  // The first hijacker should now have the events.
  EXPECT_FALSE(hijack_listener2->GetEvent(event_sp, std::chrono::seconds(0)));
  EXPECT_TRUE(hijack_listener1->GetEvent(event_sp, std::chrono::seconds(0)));
  EXPECT_TRUE(hijack_listener1->GetEvent(event_sp, std::chrono::seconds(0)));
}

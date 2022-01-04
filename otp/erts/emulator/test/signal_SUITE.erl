%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2006-2021. All Rights Reserved.
%% 
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%% 
%% %CopyrightEnd%
%%

%%%-------------------------------------------------------------------
%%% File    : signal_SUITE.erl
%%% Author  : Rickard Green <rickard.s.green@ericsson.com>
%%% Description : Test signals
%%%
%%% Created : 10 Jul 2006 by Rickard Green <rickard.s.green@ericsson.com>
%%%-------------------------------------------------------------------
-module(signal_SUITE).
-author('rickard.s.green@ericsson.com').

%-define(line_trace, 1).
-include_lib("common_test/include/ct.hrl").
-export([all/0, suite/0,init_per_suite/1, end_per_suite/1]).
-export([init_per_testcase/2, end_per_testcase/2]).

% Test cases
-export([xm_sig_order/1,
         kill2killed/1,
         contended_signal_handling/1,
         busy_dist_exit_signal/1,
         busy_dist_demonitor_signal/1,
         busy_dist_down_signal/1,
         busy_dist_spawn_reply_signal/1,
         busy_dist_unlink_ack_signal/1]).

init_per_testcase(Func, Config) when is_atom(Func), is_list(Config) ->
    [{testcase, Func}|Config].

end_per_testcase(_Func, _Config) ->
    ok.

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

suite() ->
    [{ct_hooks,[ts_install_cth]},
     {timetrap, {minutes, 2}}].

all() -> 
    [xm_sig_order,
     kill2killed,
     contended_signal_handling,
     busy_dist_exit_signal,
     busy_dist_demonitor_signal,
     busy_dist_down_signal,
     busy_dist_spawn_reply_signal,
     busy_dist_unlink_ack_signal].

%% Test that exit signals and messages are received in correct order
xm_sig_order(Config) when is_list(Config) ->
    LNode = node(),
    repeat(fun () -> xm_sig_order_test(LNode) end, 1000),
    {ok, Peer, RNode} = ?CT_PEER(),
    repeat(fun () -> xm_sig_order_test(RNode) end, 1000),
    peer:stop(Peer),
    ok.
    

xm_sig_order_test(Node) ->
    P = spawn(Node, fun () -> xm_sig_order_proc() end),
    M = erlang:monitor(process, P),
    P ! may_reach,
    P ! may_reach,
    P ! may_reach,
    exit(P, good_signal_order),
    P ! may_not_reach,
    P ! may_not_reach,
    P ! may_not_reach,
    receive
	      {'DOWN', M, process, P, R} ->
		  good_signal_order = R
	  end.

xm_sig_order_proc() ->
    receive
	may_not_reach -> exit(bad_signal_order);
	may_reach -> ok
    after 0 -> erlang:yield()
    end,
    xm_sig_order_proc().

kill2killed(Config) when is_list(Config) ->
    process_flag(trap_exit, true),
    kill2killed_test(node()),
    {ok, Peer, Node} = ?CT_PEER(),
    kill2killed_test(Node),
    peer:stop(Peer).

kill2killed_test(Node) ->
    if Node == node() ->
            io:format("Testing against local node", []);
       true ->
            io:format("Testing against remote node ~p", [Node])
    end,
    check_exit(Node, other_exit2, 1),
    check_exit(Node, other_exit2, 2),
    check_exit(Node, other_exit2, 9),
    check_exit(Node, other_exit2, 10),
    check_exit(Node, exit2, 1),
    check_exit(Node, exit2, 2),
    check_exit(Node, exit2, 9),
    check_exit(Node, exit2, 10),
    check_exit(Node, exit1, 1),
    check_exit(Node, exit1, 2),
    check_exit(Node, exit1, 9),
    check_exit(Node, exit1, 10),
    ok.

check_exit(Node, Type, N) ->
    io:format("Testing ~p length ~p~n", [Type, N]),
    P = spawn_link_line(Node, node(), Type, N, self()),
    if Type == other_exit2 ->
            receive
                {end_of_line, EOL} ->
                    exit(EOL, kill)
            end;
       true -> ok
    end,
    receive
        {'EXIT', P, Reason} ->
            if Type == exit1 ->
                    kill = Reason;
               true ->
                    killed = Reason
            end
    end.

spawn_link_line(_NodeA, _NodeB, other_exit2, 0, Tester) ->
    Tester ! {end_of_line, self()},
    receive after infinity -> ok end;
spawn_link_line(_NodeA, _NodeB, exit1, 0, _Tester) ->
    exit(kill);
spawn_link_line(_NodeA, _NodeB, exit2, 0, _Tester) ->
    exit(self(), kill);
spawn_link_line(NodeA, NodeB, Type, N, Tester) ->
    spawn_link(NodeA,
               fun () ->
                       spawn_link_line(NodeB, NodeA, Type, N-1, Tester),
                       receive after infinity -> ok end
               end).

contended_signal_handling(Config) when is_list(Config) ->
    %%
    %% Test for a race in signal handling of a process.
    %%
    %% When executing dirty, a "dirty signal handler"
    %% process will handle signals for the process. If
    %% the process stops executing dirty while the dirty
    %% signal handler process is handling signals on
    %% behalf of the process, both the dirty signal handler
    %% process and the process itself might try to handle
    %% signals for the process at the same time. There used
    %% to be a bug that caused both processes to enter the
    %% signal handling code simultaneously when the main
    %% lock of the process was temporarily released during
    %% signal handling (see GH-4885/OTP-17462/PR-4914).
    %% Currently the main lock is only released when the
    %% process receives an 'unlock' signal from a port,
    %% and then responds by sending an 'unlock-ack' signal
    %% to the port. This testcase tries to massage that
    %% scenario. It is quite hard to cause a crash even
    %% when the bug exists, but this testcase at least
    %% sometimes causes a crash when the bug is present.
    %%
    process_flag(priority, high),
    Drv = unlink_signal_drv,
    ok = load_driver(Config, Drv),
    try
        contended_signal_handling_test(Drv, 250)
    after
        ok = erl_ddll:unload_driver(Drv)
    end,
    ok.

contended_signal_handling_test(_Drv, 0) ->
    ok;
contended_signal_handling_test(Drv, N) ->
    Ports = contended_signal_handling_make_ports(Drv, 100, []),
    erlang:yield(),
    contended_signal_handling_cmd_ports(Ports),
    erts_debug:dirty_cpu(wait, rand:uniform(5)),
    wait_until(fun () -> Ports == Ports -- erlang:ports() end),
    contended_signal_handling_test(Drv, N-1).

contended_signal_handling_cmd_ports([]) ->
    ok;
contended_signal_handling_cmd_ports([P|Ps]) ->
    P ! {self(), {command, ""}},
    contended_signal_handling_cmd_ports(Ps).

contended_signal_handling_make_ports(_Drv, 0, Ports) ->
    Ports;
contended_signal_handling_make_ports(Drv, N, Ports) ->
    Port = open_port({spawn, Drv}, []),
    true = is_port(Port),
    contended_signal_handling_make_ports(Drv, N-1, [Port|Ports]).

busy_dist_exit_signal(Config) when is_list(Config) ->
    BusyTime = 1000,
    {ok, BusyChannelPeer, BusyChannelNode} = ?CT_PEER(),
    {ok, OtherPeer, OtherNode} = ?CT_PEER(["-proto_dist", "gen_tcp"]),
    Tester = self(),
    Exiter = spawn(BusyChannelNode,
                   fun () ->
                           pong = net_adm:ping(OtherNode),
                           Tester ! {self(), alive},
                           receive after infinity -> ok end
                   end),
    receive {Exiter, alive} -> ok end,
    Linker = spawn_link(OtherNode,
                        fun () ->
                                process_flag(trap_exit, true),
                                link(Exiter),
                                receive
                                    {'EXIT', Exiter, Reason} ->
                                        tester_killed_me = Reason,
                                        Tester ! {self(), got_exiter_exit_message};
                                    Unexpected ->
                                        exit({unexpected_message, Unexpected})
                                end
                         end),
    make_busy(BusyChannelNode, OtherNode, 1000),
    exit(Exiter, tester_killed_me),
    receive
        {Linker, got_exiter_exit_message} ->
            unlink(Linker),
            ok
    after
        BusyTime*2 ->
            ct:fail(missing_exit_signal)
    end,
    peer:stop(BusyChannelPeer),
    peer:stop(OtherPeer),
    ok.

busy_dist_demonitor_signal(Config) when is_list(Config) ->
    BusyTime = 1000,
    {ok, BusyChannelPeer, BusyChannelNode} = ?CT_PEER(),
    {ok, OtherPeer, OtherNode} = ?CT_PEER(["-proto_dist", "gen_tcp"]),
    Tester = self(),
    Demonitorer = spawn(BusyChannelNode,
                        fun () ->
                                pong = net_adm:ping(OtherNode),
                                Tester ! {self(), alive},
                                receive
                                    {Tester, monitor, Pid} ->
                                        _Mon = erlang:monitor(process, Pid)
                                end,
                                receive after infinity -> ok end
                        end),
    receive {Demonitorer, alive} -> ok end,
    Demonitoree = spawn_link(OtherNode,
                             fun () ->
                                     wait_until(fun () ->
                                                        {monitored_by, MB1}
                                                            = process_info(self(),
                                                                           monitored_by),
                                                        lists:member(Demonitorer, MB1)
                                                end),
                                     Tester ! {self(), monitored},
                                     wait_until(fun () ->
                                                        {monitored_by, MB2}
                                                            = process_info(self(),
                                                                           monitored_by),
                                                        not lists:member(Demonitorer, MB2)
                                                end),
                                     Tester ! {self(), got_demonitorer_demonitor_signal}
                             end),
    Demonitorer ! {self(), monitor, Demonitoree},
    receive {Demonitoree, monitored} -> ok end,
    make_busy(BusyChannelNode, OtherNode, 1000),
    exit(Demonitorer, tester_killed_me),
    receive
        {Demonitoree, got_demonitorer_demonitor_signal} ->
            unlink(Demonitoree),
            ok
    after
        BusyTime*2 ->
            ct:fail(missing_demonitor_signal)
    end,
    peer:stop(BusyChannelPeer),
    peer:stop(OtherPeer),
    ok.

busy_dist_down_signal(Config) when is_list(Config) ->
    BusyTime = 1000,
    {ok, BusyChannelPeer, BusyChannelNode} = ?CT_PEER(),
    {ok, OtherPeer, OtherNode} = ?CT_PEER(["-proto_dist", "gen_tcp"]),
    Tester = self(),
    Exiter = spawn(BusyChannelNode,
                   fun () ->
                           pong = net_adm:ping(OtherNode),
                           Tester ! {self(), alive},
                           receive after infinity -> ok end
                   end),
    receive {Exiter, alive} -> ok end,
    Monitorer = spawn_link(OtherNode,
                        fun () ->
                                process_flag(trap_exit, true),
                                Mon = erlang:monitor(process, Exiter),
                                receive
                                    {'DOWN', Mon, process, Exiter, Reason} ->
                                        tester_killed_me = Reason,
                                        Tester ! {self(), got_exiter_down_message};
                                    Unexpected ->
                                        exit({unexpected_message, Unexpected})
                                end
                         end),
    make_busy(BusyChannelNode, OtherNode, 1000),
    exit(Exiter, tester_killed_me),
    receive
        {Monitorer, got_exiter_down_message} ->
            unlink(Monitorer),
            ok
    after
        BusyTime*2 ->
            ct:fail(missing_down_signal)
    end,
    peer:stop(BusyChannelPeer),
    peer:stop(OtherPeer),
    ok.

busy_dist_spawn_reply_signal(Config) when is_list(Config) ->
    BusyTime = 1000,
    {ok, BusyChannelPeer, BusyChannelNode} = ?CT_PEER(),
    {ok, OtherPeer, OtherNode} = ?CT_PEER(["-proto_dist", "gen_tcp"]),
    Tester = self(),
    Spawner = spawn_link(OtherNode,
                         fun () ->
                                 pong = net_adm:ping(BusyChannelNode),
                                 Tester ! {self(), ready},
                                 receive {Tester, go} -> ok end,
                                 ReqID = spawn_request(BusyChannelNode,
                                                       fun () -> ok end,
                                                       []),
                                 receive
                                     {spawn_reply, ReqID, Result, _Pid} ->
                                         ok = Result,
                                         Tester ! {self(), got_spawn_reply_message};
                                     Unexpected ->
                                         exit({unexpected_message, Unexpected})
                                 end
                         end),
    receive {Spawner, ready} -> ok end,
    make_busy(BusyChannelNode, OtherNode, 1000),
    Spawner ! {self(), go},
    receive
        {Spawner, got_spawn_reply_message} ->
            unlink(Spawner),
            ok
    after
        BusyTime*2 ->
            ct:fail(missing_spawn_reply_signal)
    end,
    peer:stop(BusyChannelPeer),
    peer:stop(OtherPeer),
    ok.

-record(erl_link, {type,           % process | port | dist_process
                   pid = [],
                   state,          % linked | unlinking
                   id}).

busy_dist_unlink_ack_signal(Config) when is_list(Config) ->
    BusyTime = 1000,
    {ok, BusyChannelPeer, BusyChannelNode} = ?CT_PEER(),
    {ok, OtherPeer, OtherNode} = ?CT_PEER(["-proto_dist", "gen_tcp"]),
    Tester = self(),
    Unlinkee = spawn(BusyChannelNode,
                     fun () ->
                             pong = net_adm:ping(OtherNode),
                             Tester ! {self(), alive},
                             receive after infinity -> ok end
                     end),
    receive {Unlinkee, alive} -> ok end,
    Unlinker = spawn_link(OtherNode,
                          fun () ->
                                  erts_debug:set_internal_state(available_internal_state, true),
                                  link(Unlinkee),
                                  #erl_link{type = dist_process,
                                            pid = Unlinkee,
                                            state = linked} = find_proc_link(self(),
                                                                             Unlinkee),
                                  Tester ! {self(), ready},
                                  receive {Tester, go} -> ok end,
                                  unlink(Unlinkee),
                                  #erl_link{type = dist_process,
                                            pid = Unlinkee,
                                            state = unlinking} = find_proc_link(self(),
                                                                                Unlinkee),
                                  wait_until(fun () ->
                                                     false == find_proc_link(self(),
                                                                             Unlinkee)
                                             end),
                                  Tester ! {self(), got_unlink_ack_signal}
                          end),
    receive {Unlinker, ready} -> ok end,
    make_busy(BusyChannelNode, OtherNode, 1000),
    Unlinker ! {self(), go},
    receive
        {Unlinker, got_unlink_ack_signal} ->
            unlink(Unlinker),
            ok
    after
        BusyTime*2 ->
            ct:fail(missing_unlink_ack_signal)
    end,
    peer:stop(BusyChannelPeer),
    peer:stop(OtherPeer),
    ok.

%%
%% -- Internal utils --------------------------------------------------------
%%

load_driver(Config, Driver) ->
    DataDir = proplists:get_value(data_dir, Config),
    case erl_ddll:load_driver(DataDir, Driver) of
        ok ->
            ok;
        {error, Error} = Res ->
            io:format("~s\n", [erl_ddll:format_error(Error)]),
            Res
    end.

wait_until(Fun) ->
    case (catch Fun()) of
        true ->
            ok;
        _ ->
            receive after 1 -> ok end,
            wait_until(Fun)
    end.

find_proc_link(Pid, To) when is_pid(Pid), is_pid(To) ->
    lists:keyfind(To,
                  #erl_link.pid,
                  erts_debug:get_internal_state({link_list, Pid})).

make_busy(OnNode, ToNode, Time) ->
    Parent = self(),
    Fun = fun () ->
                  Proxy = self(),
                  Sspndr = spawn_link(
                             ToNode,
                             fun () ->
                                     IC = find_gen_tcp_input_cntrlr(OnNode),
                                     erlang:suspend_process(IC),
                                     Proxy ! {self(), input_cntrlr_suspended},
                                     receive
                                         {Proxy, resume_input_cntrlr} ->
                                             erlang:resume_process(IC)
                                     end,
                                     Proxy ! {self(), input_cntrlr_resumed}
                             end),
                  receive
                      {Sspndr, input_cntrlr_suspended} ->
                          ok
                  end,
                  Spammer = spawn_link(
                              OnNode,
                              fun () ->
                                      spammed = spam(ToNode),
                                      Proxy ! {self(), channel_busy},
                                      receive
                                      after Time -> ok
                                      end,
                                      Proxy ! {self(), timeout}
                              end),
                  receive
                      {Spammer, channel_busy} ->
                          Parent ! {self(), channel_busy}
                  end,
                  receive
                      {Spammer, timeout} ->
                          Sspndr ! {self(), resume_input_cntrlr}
                  end,
                  receive
                      {Sspndr, input_cntrlr_resumed} ->
                          ok
                  end
          end,
    Proxy = spawn_link(Fun),
    receive
        {Proxy, channel_busy} ->
            ok
    end,
    Proxy.

find_gen_tcp_input_cntrlr(Node) when is_atom(Node) ->
    case lists:keyfind(Node, 1, erlang:system_info(dist_ctrl)) of
        {Node, DistCtrl} ->
            find_gen_tcp_input_cntrlr(DistCtrl);
        false ->
            undefined
    end;
find_gen_tcp_input_cntrlr(DistCtrl) when is_pid(DistCtrl) ->
    {links, LList} = process_info(DistCtrl, links),
    try
        lists:foreach(fun (Pid) ->
                              case process_info(Pid, initial_call) of
                                  {initial_call,
                                   {gen_tcp_dist,dist_cntrlr_input_setup,3}} ->
                                      throw({input_ctrlr, Pid});
                                  _ ->
                                      ok
                              end
                      end,
                      LList),
        undefined
    catch
        throw:{input_ctrlr, DistInputCtrlr} ->
            DistInputCtrlr
    end.

spam(Node) ->
    To = {'__a_name_hopefully_not_registered__', Node},
    Data = lists:seq(1, 100),
    spam(To, Data).

spam(To, Data) ->
    case erlang:send(To, Data, [nosuspend]) of
        nosuspend ->
            spammed;
        _ ->
            spam(To, Data)
    end.

repeat(_Fun, N) when is_integer(N), N =< 0 ->
    ok;
repeat(Fun, N) when is_integer(N)  ->
    Fun(),
    repeat(Fun, N-1).

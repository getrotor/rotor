rotor
=====

An OTP application

Build
-----
```
[varoun@ip-10-0-2-194 rotor]$ rebar3 shell                                                                                                                            
===> Verifying dependencies...
===> Compiling rotor
Erlang/OTP 18 [erts-7.0] [source-4d83b58] [64-bit] [async-threads:0] [hipe] [kernel-poll:false]

Eshell V7.0  (abort with ^G)
1> ConfigList = config_file:build_config().
[{config_http,"www.bsd.org",http,"/",round_robin,30000,2000,
              ["8.8.178.110","149.20.53.86","129.128.5.194","8.8.8.8"]}]
2> [rotation_supervisor:start_in_shell_for_testing(Config) || Config <- ConfigList].
[true]
3> resolver:gethostbyname("www.bsd.org").
ns_tryagain
4> resolver:gethostbyname("www.bsd.org").
ns_tryagain
5> resolver:gethostbyname("www.bsd.org").
ns_tryagain
6> resolver:gethostbyname("www.bsd.org").
ns_tryagain
7> resolver:gethostbyname("www.bsd.org").
ns_tryagain
8> resolver:gethostbyname("www.bsd.org").
{ns_success,"8.8.178.110"}
9> resolver:gethostbyname("www.bsd.org").
{ns_success,"149.20.53.86"}
10> resolver:gethostbyname("www.bsd.org").
{ns_success,"129.128.5.194"}
11> resolver:gethostbyname("www.bsd.org").
{ns_success,"8.8.178.110"}
12> resolver:gethostbyname("www.bsd.org").
{ns_success,"149.20.53.86"}
13> resolver:gethostbyname("www.bsd.org").
{ns_success,"129.128.5.194"}
14> 

```

Start the UDP server
--------------------
```
11> udp_server:start_link([{port, 6789}]).
{ok,<0.80.0>}
12>


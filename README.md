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
1> rr("include/common.hrl").
[config_http]
2> ConfigList = config_file:build_config().
[#config_http{rotation = "www.bsd.org",check_type = http,
              check_url = "/",algorithm = round_robin,frequency = 30000,
              timeout = 2000,
              reals = ["8.8.178.110","149.20.53.86","129.128.5.94",
                       "8.8.8.8"]}]
3> [resolver:start_link(Config) || Config <- ConfigList].
[{ok,<0.77.0>}]
4> resolver:gethostbyname("www.bsd.org").
ns_tryagain
5> resolver:gethostbyname("www.bsd.org").
{ns_success,"8.8.178.110"}
6> resolver:gethostbyname("www.bsd.org").
{ns_success,"149.20.53.86"}
7> resolver:gethostbyname("www.bsd.org").
{ns_success,"8.8.178.110"}
8> resolver:gethostbyname("www.bsd.org").
{ns_success,"149.20.53.86"}
9> resolver:gethostbyname("www.bsd.org").
{ns_success,"8.8.178.110"}
10> q().
ok
11> [varoun@ip-10-0-2-194 rotor]$ 

```

Start the UDP server
--------------------
```
11> udp_server:start_link([{port, 6789}]).
{ok,<0.80.0>}
12>


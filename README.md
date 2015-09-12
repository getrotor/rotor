rotor
=====

An OTP application

Build
-----
```
varoun@ip-10-0-2-24:~/dev/rotor % rebar3 shell
===> Verifying dependencies...
===> Compiling rotor
Erlang/OTP 18 [erts-7.0.2] [source] [64-bit] [async-threads:0] [hipe] [kernel-poll:false] [dtrace]

Eshell V7.0.2  (abort with ^G)
1> resolver:start_link([{rotation, "www.bsd.org"}, {check_type, http}, {check_url, "/"}, {frequency, 30000}, {timeout, 2000}, {reals, ["8.8.178.110", "149.20.53.86", "129.128.5.194", "8.8.8.8"]}]).
{ok,<0.79.0>}
2> resolver:gethostbyname("www.bsd.org").
ns_tryagain
3> resolver:gethostbyname("www.bsd.org").
{ns_success,"8.8.178.110"}
4> resolver:gethostbyname("www.bsd.org").
{ns_success,"149.20.53.86"}
5> resolver:gethostbyname("www.bsd.org").
{ns_success,"129.128.5.194"}
6> resolver:gethostbyname("www.bsd.org").
{ns_success,"8.8.178.110"}
7> resolver:gethostbyname("www.bsd.org").
{ns_success,"149.20.53.86"}
8> resolver:gethostbyname("www.bsd.org").
{ns_success,"129.128.5.194"}
9> resolver:gethostbyname("www.bsd.org").
{ns_success,"8.8.178.110"}
10> q().
ok
11> varoun@ip-10-0-2-24:~/dev/rotor %

```

Start the UDP server
--------------------
```
11> udp_server:start_link([{port, 6789}]).
{ok,<0.80.0>}
12>


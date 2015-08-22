rotor
=====

An OTP application

Build
-----

	varoun@ip-10-0-2-24:~/dev/rotor % rebar3 shell
	===> Verifying dependencies...
	===> Compiling rotor
	Erlang/OTP 18 [erts-7.0.2] [source] [64-bit] [async-threads:0] [hipe] [kernel-poll:false] [dtrace]

	Eshell V7.0.2  (abort with ^G)
	1> check_http:start_link([{real, "www.freebsd.org"}, {path, "/"}, {timeout, 5000}]).
	{ok,<0.78.0>}
	2> check_http:check("www.freebsd.org").
	healthy
	3>


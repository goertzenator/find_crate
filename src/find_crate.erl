-module(find_crate).

-export([find_library/3, find_executable/3]).
-export([find_library/2, find_executable/2]).

%% @doc Find default Rust shared library in application priv dir that was built by rust.mk or rebar3 Rust plugin.
-spec find_library(App::atom(), CrateName::string()) ->
	{ok, Lib::string()} | {error, Reason::atom()}.
find_library(App, CrateName) -> find_library(App, CrateName, CrateName).

%% @doc Find Rust shared library in application priv dir that was built by rust.mk or rebar3 Rust plugin.
-spec find_library(App::atom(), CrateName::string(), LibName::string()) ->
	{ok, Lib::string()} | {error, Reason::atom()}.
find_library(App, CrateName, LibName) ->
	PrivDir = code:priv_dir(App),
	Wildcard = PrivDir ++ "/crates/" ++ CrateName ++ "/{lib,}" ++ LibName ++ ".{so,dll}",
	case filelib:wildcard(Wildcard) of
		[Lib] -> {ok, filename:rootname(Lib)};
		[] -> {error, not_found};
		_ -> {error, multiple_matches}
	end.

%% @doc Find default Rust executable in application priv dir that was built by rust.mk or rebar3 Rust plugin.
-spec find_executable(App::atom(), CrateName::string()) ->
	{ok, Exe::string()} | {error, Reason::atom()}.
find_executable(App, CrateName) ->find_executable(App, CrateName, CrateName).

%% @doc Find Rust executable in application priv dir that was built by rust.mk or rebar3 Rust plugin.
-spec find_executable(App::atom(), CrateName::string(), LibName::string()) ->
	{ok, Exe::string()} | {error, Reason::atom()}.
find_executable(App, CrateName, ExeName) ->
	PrivDir = code:priv_dir(App),
	Wildcard = PrivDir ++ "/crates/" ++ CrateName ++ "/" ++ ExeName ++ "{.exe,}",
	case filelib:wildcard(Wildcard) of
		[Exe] -> {ok, Exe};
		[] -> {error, not_found};
		_ -> {error, multiple_matches}
	end.

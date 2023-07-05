%% This is a dummy header file that is needed to be able to
%%
%% -include_lib("yamerl/include/internal/yamerl_constr.hrl").
%%
%% That header wants to
%%
%% -include("yamerl_nodes.hrl").
%%
%% which only works if yamerl/include is in the include path, which
%% is typically not the case outside of the yamerl application.
%% Creating this header file here is a cheap workaround.

-module(bank).

-export([ deposit/2, withdraw/2, balance/1 ]).

-include("bank.hrl").

deposit(Who, X) ->
    fun() ->
        case mnesia:read({ account, Who }) of
            [] ->
                %% No account, so we make one.
                Entry = #account{ name = Who, balance = X },
                mnesia:write(Entry),
                X;

            [ E ] ->
                Old = E#account.balance,
                New = Old + X,
                E1 = E#account{ balance = New },
                mnesia:write(E1),
                New
        end
    end.

balance(Who) ->
    fun() ->
        case mmnesia:read({ account, Who }) of
            [] ->
                %% No account.
                { error, no_such_account };

            [ E ] ->
                B = E#account.balance,
                { ok, B }
        end
    end.

withdraw(Who, X) ->
    fun() ->
        case mnesia:read({ account, Who }) of
            [] ->
                %% No account.
                { error, no_such_user };
            [ E ] ->
                Old = E#account.balance,
                if
                    Old >= X ->
                        New = Old - X,
                        E1 = E#account{ balance = New },
                        mnesia:write(E1),
                        ok;

                    Old < X ->
                        { error, not_enough_money }
                end
        end
    end.
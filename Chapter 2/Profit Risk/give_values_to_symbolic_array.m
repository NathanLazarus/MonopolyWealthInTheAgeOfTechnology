function give_values_to_symbolic_array(varname, values, n_symbolic_vars)
sym_array = sym(varname,[1 n_symbolic_vars]);
for i = 1:n_symbolic_vars
    eval(sprintf('%s=values(%d);',char(sym_array(i)),i))
end
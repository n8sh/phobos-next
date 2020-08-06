alias X(T) = T;
alias Y = X!(int);

template Z(T) { alias Z = T; }
alias W = Z!int;

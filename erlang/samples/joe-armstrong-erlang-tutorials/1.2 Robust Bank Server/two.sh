erl -pa `pwd`                              \
		-sname two -mnesia dir '"two"'         \
		-s mnesia start                      \
		-s robust_bank_manager create_table  \
		-s robust_bank_server bstart 3030    \
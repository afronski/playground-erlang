erl -pa `pwd`                              \
		-sname one -mnesia dir '"one"'         \
		-s robust_bank_manager create_schema \
		-s robust_bank_manager create_table  \
		-s mnesia start                      \
		-s robust_bank_server bstart 3020    \
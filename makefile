md4.svg:
	# stack exec FA -- -e 2 -b2014-08-19 -a2015-05-01 -w -t -- -o $@
	# stack exec FA -- -e 2 -b2014-08-19 -a2015-05-01 -w -- -o $@ > $@.log
	stack exec FA -- -e 2 -b2015-05-01 -a2016-08-18 -w -- -o $@ > $@.log

md-c.svg:
	stack exec FA -- -e 2 -b2015-04-31 -a2015-05-01 -w -t -- -o $@
md-y.svg:
	stack exec FA -- -e 2 -p'Year 5 1' -w -t -- -o $@

md-ss15.svg:
	stack exec FA -- -e 2 -b2014-08-19 -a2015-08-31 -w -- -o $@


md-ss17.svg:
	stack exec FA -- -e 2 -b2016-08-18 -a2017-08-31 -w -- -o $@
md-ss17-t.svg:
	stack exec FA -- -e 2 -b2016-08-18 -a2017-08-31 -w -t -- -o $@

md-ss18.svg:
	stack exec FA -- -e 2 -b2018-08-01 -a2019-07-31 -w -- -o $@
md-ss18-t.svg:
	stack exec FA -- -e 2 -b2018-08-01 -a2019-07-31 -w -t -- -o $@

paye-2014.svg:
	stack exec FA -- -e 27 -b2014-04-06 -a2015-04-05 -w -p'Month 5' -- -o $@ > $@.log

ghcid:
	ghcid --command="stack ghci"

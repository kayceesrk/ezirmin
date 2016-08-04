all:
	ocaml pkg/pkg.ml build

doc:
	topkg doc

clean:
	find . -name *~ | xargs rm -f
	rm -rf _build slacklogger slacklogger.install

.PHONY: doc

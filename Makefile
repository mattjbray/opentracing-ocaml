build:
	jbuilder build

_opam:
	opam switch create . --empty

setup: _opam
	opam install -y ocaml-base-compiler.4.06.0 merlin utop ocp-indent
	opam install -y . --deps-only --with-test

API_KEY=

run-datadog-agent:
	docker run -d --name dd-agent \
	  -v /var/run/docker.sock:/var/run/docker.sock:ro \
	  -v /proc/:/host/proc/:ro \
	  -v /sys/fs/cgroup/:/host/sys/fs/cgroup:ro \
	  -e API_KEY=${API_KEY} \
	  -e DD_APM_ENABLED=true \
	  -e TAGS="env:local" \
	  -p 8126:8126/tcp \
	  datadog/docker-dd-agent

stop-datadog-agent:
	docker stop dd-agent
	docker rm dd-agent

run-example:
	jbuilder exec -- example_trace

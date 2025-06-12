swc \
  -C jsc.parser.syntax=typescript \
  -C jsc.parser.decorators=true \
  -C jsc.transform.decoratorVersion='2022-03' \
  -C "jsc.baseUrl=\"$(pwd)\"" \
  -C 'jsc.paths.node:fs=["./shim/fs.ts"]' \
  -C 'jsc.paths.fs=["./shim/fs.ts"]' \
  -C env.targets.node=23 \
  -d dist \
  --out-file-extension ts \
  src shim \
&& bun build \
  --target node \
  -e wasi:clocks/monotonic-clock@0.2.5 \
  -e wasi:filesystem/preopens@0.2.5 \
  -e wasi:filesystem/types@0.2.5 \
  --outfile dist/bundle.js \
  dist/src/lib.js \
&& jco componentize \
  --wit wit/ \
  --world-name ifc \
  --disable http \
  --out dist/org_agent.wasm \
  dist/bundle.js

# runpurs

PureScript corefn interpreter experiment for processing JSON

Invokes `purs compile` to generate corefn output from an expression and then interprets the result in the context of any JSON piped to standard input.

For example,

```bash
# Find GitHub repositories which are forks 
curl https://api.github.com/users/paf31/repos \
  \ | runpurs '\input -> map (\{name} -> name) (filter _.fork input)'
```

Use at your own risk.

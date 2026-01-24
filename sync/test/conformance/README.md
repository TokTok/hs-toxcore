# Sync conformance cases

Data defining what every `doc/account-sync.md` (in rs-toxcore-c) implementation
must compute. Cases live in rs-toxcore-c's `sync/conformance`, run by
`sync/tests/conformance_test.rs`. bTox's `tools/update-sync-conformance`
copies them to `test/sync/conformance`, run by `test/sync/conformance_test.dart`.

`tox_sync` is the reference. Write a case's inputs, then run:

```
BLESS=1 bazel run //rs-toxcore-c:tox_sync_conformance_test
```

to fill in its results, which are reviewed before committing.

## Values

Values are JSON:

- Bytes, keys, and IDs are lowercase hex.
- Enums are `"tag"` (fieldless) or `{"tag": fields}`, using `account-sync.md`
  names in `snake_case` (`{"notice": {"text": "hi"}}`, `"account"`,
  `{"friend": "<key>"}`). Tox values use names like `"udp"`. Message kinds
  are `"normal"` and `"action"`.
- Absent options are `null`.
- Unknown value kinds (or known kinds with an unknown Tox value or
  chat-invite kind) become `{"unknown": kind}` and show nothing. An unknown
  `PictureFormat` becomes `"original"`, matching its behaviour.
- Messages a version ignores (including unknown call actions) are `null`.
  Undecodable data is `"error"`.

## Labels

Behaviour cases assign labels to components. A label can be used anywhere its
value is expected:

| Label    | Value            | From                                                                                                              |
| -------- | ---------------- | ----------------------------------------------------------------------------------------------------------------- |
| `%name`  | a key            | the file's `keys`                                                                                                 |
| `&name`  | a `ChatId`       | the file's `chats`                                                                                                |
| `$name`  | a graph's ID     | the file's `graphs`: `{"account": key}`, `{"friend": [account, friend]}`, `{"group": id}` or `{"conference": id}` |
| `@label` | an event's ID    | the case's `events`                                                                                               |
| `#label` | an event's bytes | the case's `events`                                                                                               |

A case's `events` are listed in order, each after any event it references:

- Events override the file's `defaults`. Unless given, `parents` is empty and
  `rank` is `0` (or `1` + the highest preceding parent's rank).
- `{"chain": "c", "count": n}` creates `n` notices (`c0` to `c{n-1}`), each
  parenting the next, starting after `after` if given. Notice `i` has text
  `"i"` and time `i`. A chain's name stands for all its notices.
- `{"raw": hex}` is raw bytes (not necessarily an event).

## Results

Unordered protocol lists are sorted for comparison: replies, `Summary` entries
(plus their heads and `Want` IDs), a `History`'s `before`, and account state.
Events stay in send order.

Account state is produced by the spec's merge rules over its events: wanted
friends or friend requests, kept facts, merges, relay bots, reads, settings
(with their encoding), and invites. Folding the same events in any order must
yield the same state.

## Files

- `codec.json`:
  - graph, blob, and heads IDs
  - encoding and decoding events and session messages, plus decoding
    older, newer, and malformed bytes
  - packets
  - editing a setting kept from a newer version
  - pairing links
- `state.json`: account states and updated live facts.
- `store.json`: accepted events, heads, where history stops, and display
  order.
- `session.json`: store answers and step-by-step replica runs: `receive` a
  message, go `offline`, `author` an event (pushed to the home), `home_add`
  events to the home, and `converse` with it until done, starting from a
  `"session"` (`Hello` and the home's `Summary`) or given messages. The home
  answers from its store (including `History`), and `"role": "home"` answers
  are only the home's to give.

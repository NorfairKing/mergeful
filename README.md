# Mergefull

A way to synchronise items with safe merge conflicts.

## Related project

If the collection that you are syncing has immutable items, you will be better off using [mergeless](https://github.com/NorfairKing/mergeless#readme) instead.

## Implementation overview

Situation: Multiple clients (for example a note synchronisation app) want to sync with a server storing state for them.
Goal: Get clients to agree on the collection of items and prevent that one client accidentally overrides a value saved by another client.
Solution: Store a server-side name and monotonically increasing number (revision number of the data, or current time) for each number.

See the following blogposts for (many) more details:

- https://cs-syd.eu/posts/2019-10-14-mergeful-value
- https://cs-syd.eu/posts/2019-11-28-mergeful-item
- https://cs-syd.eu/posts/2019-12-28-mergeful-collection

or the talk at Haskellerz:

https://www.youtube.com/watch?v=MkbhHmAk47k

---
title: Mergeful: Cooperative agreement
author: Tom Sydney Kerckhove
---

# Mergeful: Cooperative agreement

# Agreement

After talking once, we have the same X.

Relaxed:

After some finite amount of talking, we will have the same X.

# Examples

- Google docs: online
- Git: patch-based
- Bitcoin: Ledger
- Dropbox: state-based

# Agreement

- If you change your X locally, we can agree on that change.
- If we change our X locally, we can find common ground.
- If we change our X locally, we do not lose data.
- If we do not change anything, we do not need to talk (much).


# Mergeful

```
        Central server
        /   |         \
client 1  client 2     client 3
  CLI    Web Interface    GUI   
```

# Agreeing on a value

```
ClientValue                              ServerValue
 |\                                         |
 | makeSyncRequest -> SyncRequest           |
 |                            \             |
 |                            processServerSync
 |                            /             |
mergeSyncResponse <-  SyncResponse          |
 |                                       ServerValue
ClientValue
```


# Server time

``` haskell
newtype ServerTime = ServerTime { unServerTime :: Word64 }

data Timed a = Timed
  { timedValue :: a
  , timedTime :: ServerTime
  }
```


# Client value

``` haskell
data ClientValue a
  = ClientValue (Timed a) ChangedFlag

data ChangedFlag
  = Changed
  | NotChanged
```


# Server value

``` haskell
data ServerValue a
  = ServerValue (Timed a)
```

# Sync request

``` Haskell
data ValueSyncRequest a
  = Known ServerTime
  | Changed (Timed a)
  deriving (Show, Eq, Generic)
```


# Sync response

``` Haskell
data ValueSyncResponse a
  = InSync
  | ClientChanged ServerTime
  | ServerChanged (Timed a)
  | Conflict !(Timed a)
  deriving (Show, Eq, Generic)
```


# makeSyncRequest

``` Haskell
makeSyncRequest :: ClientValue a -> SyncRequest a
makeSyncRequest (Client t cf) =
  case cf of
    NotChanged -> Known (timedTime t)
    Changed -> Changed t
```

# processServerSync

``` 
Known
  -> Older than server?
    Yes -> Server changed
    No -> In sync

Changed
  -> Older than server?
    Yes -> Conflict
    No -> Client changed
```

# mergeSyncResponse

Left as an exercise to the reader

``` Haskell
mergeValueSyncResponseRaw
  :: (a -> a -> a) -- Handling conflicts
  -> ClientValue a -> SyncResponse a -> ClientValue a
```

# Handling conflicts

1. Client wins  (never lose data ; desync)

. . .

2. Server wins  (always in sync ; lose data)

. . .

3. GADT

  ``` haskell
  type ConflictResolutionStrategy a = a -> a -> a
  ```

  * Idempotent
  * Associative
  * The same on all clients


# Agreeing on 0-1 value


```
ServerValue (Maybe a)
```

Not the same.

# Client item

``` haskell
data ClientItem a
  = Empty
  | Added !a
  | Synced !(Timed a)
  | SyncedButChanged !(Timed a)
  | Deleted !ServerTime
```

# Server item

``` haskell
data ServerItem a
  = ServerEmpty
  | ServerFull !(Timed a)
```

# Sync request


``` haskell
data ItemSyncRequest a
  = Poll
  | New !a
  | Known !ServerTime
  | KnownButChanged !(Timed a)
  | DeletedLocally !ServerTime
```

# Sync response

``` Haskell
data ItemSyncResponse a
  = InSyncEmpty
  | InSyncFull
  | ClientAdded !ServerTime
  | ClientChanged !ServerTime
  | ClientDeleted
  | ServerAdded !(Timed a)
  | ServerChanged !(Timed a)
  | ServerDeleted
  | Conflict !(Timed a)
  | ConflictClientDeleted !(Timed a)
  | ConflictServerDeleted
```

# Functions

It's complicated, 9 cases

# Agreeing on a collection of values

```
ServerValue (Bag a)
```

Not the same.

We'll use names.

* UUIDs
* SQL Id

# Client Store

``` haskell
data ClientStore i a =
  ClientStore
    { addedItems :: Map ClientId a
    , syncedItems :: Map i (Timed a)
    , syncedButChangedItems :: Map i (Timed a)
    , deletedItems :: Map i ServerTime
    }
```

# Server Store

``` haskell
newtype ServerStore i a = ServerStore (Map i (Timed a))
```

# Sync request


``` haskell
data SyncRequest i a =
  SyncRequest
    { newItems :: Map ClientId a
    , knownItems :: Map i ServerTime
    , knownButChangedItems :: Map i (Timed a)
    , deletedItems :: Map i ServerTime
    }
```

# Sync response

``` Haskell
data SyncResponse i a =
  SyncResponse
    { clientAdded :: Map ClientId (i, ServerTime)
    , clientChanged :: Map i ServerTime
    , clientDeleted :: Set i
    , serverAdded :: Map i (Timed a)
    , serverChanged :: Map i (Timed a)
    , serverDeleted :: Set i
    , conflicts :: Map i (Timed a)
    , conflictsClientDeleted :: Map i (Timed a)
    , conflictsServerDeleted :: Set i
    }
```

# Functions

It's _very_ complicated but essentially:
Match up by name, then use agreement on items.


# Testing (1)

- Validity-based testing
- Unit tests: not really
- Property test:

``` haskell
  describe "processServerValueSync" $
    it "makes no changes if the sync request reflects the state of the server" $
      forAllValid $ \i ->
        forAllValid $ \st -> do
          let store1 = ServerValue $ Timed i st
              req = ValueSyncRequestKnown st
          let (resp, store2) = processServerValueSync @Int store1 req
          store2 `shouldBe` store1
          resp `shouldBe` ValueSyncResponseInSync
```

# Testing (2)

- Property combinators:

``` haskell
  describe "processServerValueSync" $ 
    it "produces valid responses and stores" $
      producesValidsOnValids2 (processServerValueSync @Int)
```

- Test suite combinators:

``` haskell
  describe "processServerValueSync" $ 
    jsonSpecOnValid @(ValueSyncResponse Int)
    mergeFunctionSpec @Int mergeSyncResponseFromServer
```

``` haskell
mergeFunctionSpec ::
     forall a. (Show a, Ord a, GenValid a)
  => (forall i. Ord i =>
                  ClientStore i a -> SyncResponse i a -> ClientStore i a)
  -> Spec
```

# Smos & support


https://github.com/NorfairKing/mergeless
https://github.com/NorfairKing/mergeful

https://cs-syd.eu

https://cs-syd.eu/support
https://smos.cs-syd.eu/

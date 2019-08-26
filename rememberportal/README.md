### Haskell Setup

1. If you haven't already, [install Stack](https://haskell-lang.org/get-started)
	* On POSIX systems, this is usually `curl -sSL https://get.haskellstack.org/ | sh`
2. Install the `yesod` command line tool: `stack install yesod-bin --install-ghc`
3. Build libraries: `stack build`

If you have trouble, refer to the [Yesod Quickstart guide](https://www.yesodweb.com/page/quickstart) for additional detail.

### Development

Start a development server with:

```
stack exec -- yesod devel
```

As your code changes, your site will be automatically recompiled and redeployed to localhost.

### Tests

```
stack test --flag rememberportal:library-only --flag rememberportal:dev
```

## Yesod Documentation

* Read the [Yesod Book](https://www.yesodweb.com/book) online for free
* Check [Stackage](http://stackage.org/) for documentation on the packages in your LTS Haskell version, or [search it using Hoogle](https://www.stackage.org/lts/hoogle?q=). Tip: Your LTS version is in your `stack.yaml` file.
* For local documentation, use:
	* `stack haddock --open` to generate Haddock documentation for your dependencies, and open that documentation in a browser
	* `stack hoogle <function, module or type signature>` to generate a Hoogle database and search for your query
* The [Yesod cookbook](https://github.com/yesodweb/yesod-cookbook) has sample code for various needs

## FAQ

#### Some FIO payments are missing from the database. What now?

Run `curl "https://www.fio.cz/ib_api/rest/set-last-date/$FIOTOKENHERE/1970-01-01/"` to rewind last synced transaction pointer. Then run `rememberportal-sync-fio` to import ALL payments. Existing payments are left untouched, however if you deleted any payments they will be re-added.

In order to avoid this problem, never use rememberportal's Fio token for anything else.

#### Migration from original memberportal

```
INSERT INTO user
SELECT  a.id,
        a.username,
        CASE a.email WHEN '' THEN a.username || '@UNKNOWN' ELSE a.email END,
        NULL,
        NULL,
        false,
        a.first_name || ' ' || a.last_name,
        CASE b.alt_nick WHEN '' THEN NULL ELSE b.alt_nick END,
        CASE b.phone WHEN '' THEN NULL ELSE b.phone END,
        CASE b.xmpp WHEN '' THEN NULL ELSE 'xmpp:' || b.xmpp END,
        NULL,
        b.payments_id,
        a.date_joined,
        NULL,
        NULL,
        CASE b.status WHEN 'EX' THEN 'Exmember' WHEN 'RE' THEN 'Rejected' WHEN 'AC' THEN 'Accepted' WHEN 'NA' THEN 'Awaiting' END,
        b.council,
        a.is_staff
FROM      auth_user AS a
LEFT JOIN baseprofile_baseprofile AS b ON a.id = b.user_id;
```

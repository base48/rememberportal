# base48 memberportal (rewritten)

Simple web application that facilitates operation of small non-profit organizations. Helps you keep track of:

* registrations of members w/ email confirmation
* who has keys to physical space
* membership fees and payments (can download payment data from [Fio bank](https://fio.cz))

This program was written for the needs of [base48 hackerspace](https://base48.cz) and is based on
[the original memberportal](https://github.com/hackerspace/memberportal) written by @sorki
(rememberportal heavily reuses its HTML/CSS templates).

## Configuration

Please read the comments in [config/settings.yml](./config/settings.yml). This file is read at compile time.

You can customize the memberportal somewhat by editing the files in the [templates](./templates/) subdirectory.
Please note that these are evaluated at compile time as well.

## Haskell Setup - Stack

1. If you haven't already, [install Stack](https://haskell-lang.org/get-started)
   * On POSIX systems, this is usually `curl -sSL https://get.haskellstack.org/ | sh`
   * Or use your distribution's package if available
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

## Haskell Setup - Nix

With [Nix](https://nixos.org/nix/) package manager, you can use nix-shell to quickly setup development environment:

```
nix-shell
```

Once in nix-shell you can either use `cabal v2-<command>` commands:

```
cabal v2-build
cabal v2-test 
cabal v2-run rememberportal
cabal v2-run rememberportal-sync-fio --help
```

Or run the development server:

```
yesod devel
```

For production deployment this repo contains a [NixOS](https://nixos.org/nixos/) module as well as
example [nixops](https://nixos.org/nixops/) configuration.

## Yesod Documentation

* Read the [Yesod Book](https://www.yesodweb.com/book) online for free
* Check [Stackage](http://stackage.org/) for documentation on the packages in your LTS Haskell version, or [search it using Hoogle](https://www.stackage.org/lts/hoogle?q=). Tip: Your LTS version is in your `stack.yaml` file.
* For local documentation, use:
	* `stack haddock --open` to generate Haddock documentation for your dependencies, and open that documentation in a browser
	* `stack hoogle <function, module or type signature>` to generate a Hoogle database and search for your query
* The [Yesod cookbook](https://github.com/yesodweb/yesod-cookbook) has sample code for various needs

## FAQ

#### Some FIO payments are missing from the database. What now?

Run `curl "https://www.fio.cz/ib_api/rest/set-last-date/$FIOTOKENHERE/1970-01-01/"` to rewind last synced transaction pointer **Be aware that FIO does not let you load data older than 90 days by default (unless enabled under token settings in ib.fio.cz)** . Then run `rememberportal-sync-fio` to import ALL payments. Existing payments are left untouched, however if you deleted any payments they will be re-added.

In order to avoid this problem, never use rememberportal's Fio token for anything else.

#### I need to change X but there's no form to do it

You may have to edit the database manually, for example using the [sqlite
CLI](https://www.sqlite.org/cli.html):
```
cp /var/lib/rememberportal/rememberportal.sqlite3 /var/lib/rememberportal/rememberportal-backup`date "+%y%m%d"`.sqlite3
sqlite3 /var/lib/rememberportal/rememberportal.sqlite3

sqlite> .headers on
sqlite> .changes on
sqlite> .mode column
sqlite> BEGIN TRANSACTION;
sqlite> ...
sqlite> COMMIT;
```

#### How to add admin user

The first time you run rememberportal it creates an empty database. To add admin user, register as new member in the web app then add admin privileges:
```
sqlite3 /var/lib/rememberportal/rememberportal.sqlite3

sqlite> UPDATE user SET state = "Accepted", staff = 1 WHERE ident = "YourNick";
```

You don't need to have working email setup in order to register because the verification URL is written to logs.

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

UPDATE fee SET amount = CASE amount WHEN 500 THEN 400 WHEN 250 THEN 200 ELSE amount END WHERE period_start < '2016-10-05';
UPDATE fee SET amount = CASE amount WHEN 500 THEN 800 WHEN 250 THEN 400 ELSE amount END WHERE period_start > '2016-10-06' AND period_start < '2017-01-05';
```

# tronr 0.1.2

## Minor improvements

* Functions `parse_tx_info()` (non-public utility function), `get_block_info()` and `get_blocks_for_time_range()` now work a little bit faster by doing less work (e.g., by not converting the API-defined variable names to snake case).

# tronr 0.1.1

## Minor improvements and bug fixes

* `get_tx_info_by_id()` now returns nothing (`NULL`) if no data can be retrieved for the requested transaction (e.g., because the provided transaction ID is wrong; [#113](https://github.com/next-game-solutions/tronr/issues/113)).
* The `avg_block_time` and `shielded_tx` fields have been removed from the results rturned by `get_chain_statistics()`. This is because these two fields don't carry any useful information (i.e. `avg_block_time` is always 3, and `shielded_tx` is always 0; [#114](https://github.com/next-game-solutions/tronr/issues/114)).
* `to_unix_timestamp()` now returns correctly formatted character values of the converted timestamps ([#115](https://github.com/next-game-solutions/tronr/issues/115)).

## Documentation improvements

* Minor typos and grammar errors fixed in help files and vignettes.


# tronr 0.1.0 - initial release

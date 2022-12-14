# data_center
A test repository where the public data repos are submodules, to see if we can have feedback on the state of the data.

## Adding submodules
Follow [this link](https://github.blog/2016-02-01-working-with-submodules/), but generally

```shell
git submodule add https://github.com/<user>/rock rock
```

Also note that submodules are consistency > convenience, meaning there is an implicit assumption that this repository will push no changes to the submodules 

Can be possible to [push and update submoudles](https://stackoverflow.com/questions/5814319/git-submodule-push), too though
```
git push --recurse-submodules=on-demand
```

## To clone things with submodules
Reference this [stackoverflow article](https://stackoverflow.com/questions/3796927/how-do-i-git-clone-a-repo-including-its-submodules), but

```
git clone --recurse-submodules -j8 git@github.com:uva-bi-sdad/test_data_center.git
```

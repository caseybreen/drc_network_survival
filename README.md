# remote_mortality_study

Code, survey instruments, irb protocols, and other resource for DRC mortality study. 





## Google Drive folders

The data/ and out/ folders are stored in a shared Google Drive folder called 

- 'drc-mortality/data' 
- 'drc-mortality/out'. 

Dennis can give you access to this folder if you don't have it already. I recommend that you create a symlink. (The code in this repository will assume you have this symlink.) 

**On a Mac** you can do this by typing `ln -s PATH_TO_GOOGLE_DRIVE_FOLDER`. 

For example, on my mac laptop, I was in the root directory of this repository and I typed `ln -s ~/Google Drive/_drc_mortality/drc-mortality/data/`. A symlink was created, and in my scripts I can now refer to the data/ directory to get to the Google Drive folder.
I did the same thing for the out/ directory.

**On the server**, First, be sure you are on keyfitz.  Next, install [rclone](https://rclone.org/) and set it up to work with Google Drive. (Carl Boe gave me helpful advice on how to do that.) Then, clone this github repo to your account. Go into the repo and create empty `data` and `out` directories. Then mount the google drive folder for `data` to the local `data` directory, and do the same for the `out` folder.  To do this, I typed

`rclone mount --daemon gdrive:_drc_mortality/drc-mortality/out out`

and

`rclone mount --daemon gdrive:_drc_mortality/drc-mortality/data data`

The `--daemon` flag tells `rclone` to run the mounting app in the background. See the [rclone mount help page](https://rclone.org/commands/rclone_mount/) for more info.




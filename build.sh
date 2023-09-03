# Thanks for your interest in the Game :: Dangerous project.

# This script will download and build the Game :: Dangerous engine, map development server and 
# what so far exists of the corresponding web client.  The map development server comprises a 
# Haskell program with an embedded Node.js based web server to provide a local network interface.  
# Note that this server is only intended to provide a locally accessible service 
# (i.e. at http://localhost/index.html).

# Prerequisites
# Haskell Stack (recommended >= v2.7.5)
# Node.js (recommended >= v18.17.1)
# Yarn package manager is used with the web client

# After running the script you need to copy the below text into another file, uncomment it and save 
# as "config.txt" in the Game-Dangerous folder that the engine was cloned into.
# To start the engine run "Game-Dangerous-exe" from the Game-Dangerous folder.
# To start the server run "./start_server_.sh" from the Game-Dangerous folder.

# map_file_path=GD_release_4/
# map_file=map1.dan
# game_save_path=GD_release_4/Saved_games/
# shader_file=shaders.glsl
# init_u=23.5
# init_v=18.5
# init_w=0.2
# gravity=-1.5
# friction=-0.3
# run_power=180
# jump_power=1.5
# verbose_mode=n
# model_data_dir=GD_release_4/Models/
# splash_image=off
# u_limit=39
# v_limit=53
# w_limit=2
# sound_data_dir=GD_release_4/Sounds/
# prob_c=0
# frustumScale1=1.25
# version_and_platform_string=Linux x86_64 Engine version: 0.9.0 Server version: 1.0.5 (commit 7df2cd8)
# cb_PAUSE=x
# cb_FORWARD=w
# cb_BACK=s
# cb_STRAFE_LEFT=a
# cb_STRAFE_RIGHT=d
# cb_TURN_LEFT=k
# cb_TURN_RIGHT=l
# cb_JUMP=u
# cb_LIGHT_TORCH=t
# cb_SWITCH_VIEW=c
# cb_ROTATE_VIEW=v
# cb_FIRE= 
# cb_MENU_SELECT=1
# cb_MENU_BACK=2
# cb_MENU_HOME=3
# resolution_x=1280
# resolution_y=960
# min_frame_t=15384615
# on_screen_metrics=low
# music=off
# music_period=15040
# config_file=config.txt
# map_unlock_code=null
# map_id=107072021

git clone https://github.com/Mushy-pea/Game-Dangerous
git clone https://github.com/Mushy-pea/Game-Dangerous-Client
git clone https://github.com/Mushy-pea/GPLC-scripts-and-maps

cd Game-Dangerous
sudo apt install libgl1-mesa-dev # These two lines may be specific to Debian based Linux distros.
sudo apt install libglu1-mesa-dev # They work on my PopOS 22.04 system.  Please modify if necessary.
stack install --local-bin-path=.

cd node_server
npm install typescript --save-dev
npm install express
npx tsc server.ts

cd ..
cd ..
cd Game-Dangerous-Client
yarn add typescript --dev
yarn tsc

echo "If you wish to deploy the assets from Game :: Dangerous release 4 at this time, "
echo "please download GD_release_4_Linux.zip from https://github.com/Mushy-pea/Game-Dangerous/releases/tag/v0.9.0.0.  "
echo "Extract the GD_release_4 folder and place it in the Game-Dangerous folder within your base build folder.  "
echo "Then enter \"y\" at this prompt to complete the deployment.  Otherwise, enter any other string to exit here."
read answer
if [ "$answer" = "y" ]; then
  echo "Completing deployment of Game :: Dangerous release 4 assets..."
  cd ..
  cp GPLC-scripts-and-maps/Maps/map1.dan Game-Dangerous/GD_release_4/map1.dan
  cp -R GPLC-scripts-and-maps/GPLC_Programs Game-Dangerous/GD_release_4/GPLC_Programs
  echo "Build script completed."
else
  echo "Build script completed."
fi


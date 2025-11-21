####R Code for Presentation Below:####
####Making Sure Git is installed####
#Use the following code to connect to Git and then look at the version you have - must be used within the terminal
#     which git
#     git --version

#If you do not have Git, you will need to install it now

##For Windows: Download Git for Windows
#To Update Git use the following code
#     git update-git-for-windows


##For Mac: Install the Xcode command line tools by going to the shell and using the following code
#    git --version
#    git config
#Accept the installation offer and install

####To create an associated name and email to Git from R Studio####
#Use the Terminal for the following code:
#OR use the system command in front 
#  git config --global user.name "mrhand18" #Use username from Github account
#  git config --global user.email "mrhand1@live.com" #Use email from Github
#  git config --global --list

#OR Using the "usethis" package
install.packages("usethis")
library(usethis)
edit_git_config()

####Connect Account to Github Access Token####
#Generate a Token within the Github Accounts Page
#To store the token in RStudio use the following package installation:
install.packages("gitcreds")
library(gitcreds)
#Add new credentials by pasting the generated code after the following prompt:
gitcreds_set()

####Create a Git Repository from RStudio####
#Install the "Usethis" package
use_git(message = "Initial commit") #Can use without the "initial commit"
#create a new project and make sure that the create git repository is clicked

####Connect to Github Repository####
#Create a Repository in Github webpage
#Connect to it by copying the URL from the webpage and using the following code in the Terminal
#   git clone http://github.com/mrhand18/GitP.git   #Change URL to copied URL from own Repo
#   cd GitP  #Change to match own name
#   git remote show origin

#Use the same code as above to connect to the Repository for class
#     git clone https://github.com/jedrake/FOR797_Fall2025.git

####Pushing Code to Github ####
#Used to Send Code to Github
#Use the Arrow keys in Git or use method below:
system("git add .")
#or if you want to do a specific file
system("git add C:/Users/Madison/Documents/PhD/RSeminar/GitP/RCode.R")
#commit changes to repository
system("git commit -m \"Git Command Test\"")
#If you used the terminal before to make the username and email,  you may need to enter this code
system("git config --global user.name \"mrhand18\"") #Use username from Github account
system("git config --global user.email \"mrhand1@live.com\"") #Use email from Github
#Push changes
system("git push")
#Check the status of repository
system("git status")

####Pulling Code from Github ####
#To Pull Code from Github
#Can easily use the arrow keys in Git to pull or use the following code:
system ("git pull")

####Creating Branches in RStudio####
#Use the "New Branch" Button in the top right in Git or the following code:
system("git branch NewBranch") #Create a new branch
system("git checkout NewBranch") #Change between branches
#Can also use "git checkout -b <newbranchname>" to create and auto switch to the new branch
system("git status") #Check the status to confirm in branch

system("git push -u origin NewBranch") #Push the branch to remote repository

####Merging Branches####
#Much easier and visually appealing to do so through the website but there are commands too:

system("git switch main") #Switch to Main Branch - can also use 'checkout'
system("git merge TestBranch -m \"Merged TestBranch\"") #merge the branch and add a message on the merge
system("git push GitP main") #push the merge changes to the remote repository

####Using Github to Install R Packages####
#ex: installing the Distance program from the development team
install.packages("devtools")
library(devtools)
install_github("DistanceDevelopment/Distance")

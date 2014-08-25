

/api
    /login name password
    /logout

    /user
        /list
        /create name password isAdmin
        /delete name
        /update
            /password password
            /email email
            /isAdmin isAdmin

    /project projShortName
    /project
        /list
        /create projName projShortName repoLoc sshKey
        /delete projShortName
        /update
            /name projShortName name
            /shortName projShortName shortName
            /repoLoc projShortName repoLoc
            /sshKey projShortName sshKey
            /users
                /add projShortName name
                /delete projShortName name

    /patch
        /:patchId
        /list
    
    /build
        /:buildId
        /list       : 
        /patch      : build repositorys for patch
        buildId        : return build info
            /logs 
            /file-changes 
            /cancel
            /rebuild

    /collection
        /:collectionId
        /patches
        /close



/bundle
    /check projShortName patch-bundle

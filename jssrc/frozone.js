/** @jsx React.DOM */

var FrozoneBuildBadge = React.createClass({
    render: function() {
        var b = this.props.build;

        var buildStatus = <span className="label label-default">unknown</span>;
        if (b.patchCanceledOn) {
            buildStatus = <span className="label label-danger">canceled</span>;
        } else {
            if (b.buildEnqueuedOn) {
                buildStatus = <span className="label label-warning">enqueued</span>;
            }
            if (b.buildStartedOn) {
                buildStatus = <span className="label label-info">started</span>;

                if (b.buildFailedOn) {
                    buildStatus = <span className="label label-danger">failed</span>;
                } else if (b.buildSuccessOn) {
                    buildStatus = <span className="label label-success">succeded</span>;
                }
            }
        }

        return buildStatus;
    }
});

var FrozonePatchPreview = React.createClass({
    render: function() {
        var shortDisplay = this.props.build.patchBundle.split("New patches:");
        return <pre>{shortDisplay[0].trim()}</pre>;
    }
})

var FrozoneBuildRow = React.createClass({
    getInitialState: function() {
        return {
            canceled: false
        }
    },

    cancel: function(e) {
        e.preventDefault();
        var build = this.props.build;

        $.ajax({
            url: "/api/build/" + build.key + "/cancel",
            dataType: 'json',
            success: function(data) {
                this.setState({canceled: true });
            }.bind(this),
            error: function(xhr, status, err) {
                console.error("build-" + build.key + "-cancel", status, err.toString());
            }.bind(this)
        });
    },

    render: function() {
        var build = this.props.build;

        var shortDisplay = build.value.patchBundle.split("New patches:");

        var badge = <FrozoneBuildBadge build={build.value} />;
        var cancelAction = <span />;
        if (this.state.canceled || build.value.patchCanceledOn) {
            badge = <span className="label label-danger">canceled</span>;
        } else {
            cancelAction = (<a href="#" onClick={this.cancel} title="Cancel build / mark as bad patch">
                <i className="fa fa-bomb"></i>
            </a>);
        }

        return (<tr>
            <th>{build.key}</th>
            <td>{build.value.createdOn}</td>
            <td><FrozonePatchPreview build={build.value} /></td>
            <td>{badge}</td>
            <td>
                <a href={"#/build/" + build.key} title="More Information">
                    <i className="fa fa-info-circle"></i>
                </a>&nbsp;
                {cancelAction}
            </td>
            </tr>);
    }
});

var FrozoneOverview = React.createClass({
    getInitialState: function() {
        return {
            builds: []
        }
    },

    fetchData: function() {
        $.ajax({
            url: "/api/list-builds",
            dataType: 'json',
            success: function(data) {
                this.setState({ builds: data });
            }.bind(this),
            error: function(xhr, status, err) {
                console.error("list-builds", status, err.toString());
            }.bind(this)
        });
    },

    componentDidMount: function() {
        this.props.timer = setInterval(this.fetchData, 10000);
        this.fetchData();
    },

    componentWillUnmount: function() {
        clearInterval(this.props.timer);
    },

    render: function() {
        var buildRows = this.state.builds.map(function (build) {
            return (<FrozoneBuildRow key={build.key} build={build} />);
        });

        return (<table className="table table-hover">
            <thead>
            <tr>
                <th>ID</th>
                <th>Created on</th>
                <th>Patches</th>
                <th>Status</th>
                <th>Actions</th>
            </tr>
            </thead>
            <tbody>
                {buildRows}
            </tbody>
        </table>);
    }
});

var FrozoneLoading = React.createClass({
    render: function() {
        var s = {"text-align": "center"};

        return (<div style={s}><img src="/img/loading-bar.gif" /></div>);
    }
});

var FrozoneBuildDetails = React.createClass({
    getInitialState: function() {
        return {
            dataState: "loading",
            build: null
        };
    },

    fetchData: function() {
        $.ajax({
            url: "/api/build/" + this.props.buildId,
            dataType: 'json',
            success: function(data) {
                if (data.error) {
                    console.error(data.error);
                    this.setState({ dataState: "error" });
                } else {
                    this.setState({ dataState: "ok", build: data });
                }
            }.bind(this),
            error: function(xhr, status, err) {
                console.error("get-build-" + this.props.buildId, status, err.toString());
                this.setState({ dataState: "error" });
            }.bind(this)
        });
    },

    componentDidMount: function() {
        this.props.timer = setInterval(this.fetchData, 10000);
        this.fetchData();
    },

    componentWillUnmount: function() {
        clearInterval(this.props.timer);
    },

    render: function() {
        if (this.state.dataState === "loading") {
            return <FrozoneLoading />;
        } else if (this.state.dataState === "error") {
            return (<h2>404 Build not found</h2>);
        } else {
            var b = this.state.build;

            var cancelBox = <span></span>;
            if(b.patchCanceledOn) {
                cancelBox = (<div className="alert alert-danger" role="alert">
                    <strong>Patch was canceled: </strong>
                    <span>{b.patchCancelReason ? b.patchCancelReason : "Reason unknown"}</span>
                </div>);
            }

            return (<div>
            <h2><FrozoneBuildBadge build={b} /> Build #{this.props.buildId}</h2>
            {cancelBox}
            <FrozonePatchPreview build={b} />
                <table className="table">
                    <tr>
                        <th>Interested People</th>
                        <td>{b.notifyEmail.join(", ")}</td>
                    </tr>
                    <tr>
                        <th>Branch</th>
                        <td>{b.branch}</td>
                    </tr>
                    <tr>
                        <th>Created on</th>
                        <td>{b.createdOn}</td>
                    </tr>
                    <tr>
                        <th>Changes-Hash</th>
                        <td>{b.changesHash}</td>
                    </tr>
                </table>

                <h3>Lifecycle</h3>
                <table className="table">
                    <tr>
                        <th>Enqueued on</th>
                        <td>{b.buildEnqueuedOn}</td>
                    </tr>
                    <tr>
                        <th>Build started on</th>
                        <td>{b.buildStartedOn}</td>
                    </tr>
                    <tr>
                        <th>Build failed on</th>
                        <td>{b.buildFailedOn ? b.buildFailedOn : ""}</td>
                    </tr>
                    <tr>
                        <th>Build success on</th>
                        <td>{b.buildSuccessOn ? b.buildSuccessOn : ""}</td>
                    </tr>
                </table>

                <h3>Docker</h3>
                <table className="table">
                    <tr>
                        <th>Base-Image</th>
                        <td>{b.dockerBaseImage ? b.dockerBaseImage : "-"}</td>
                    </tr>
                    <tr>
                        <th>Image</th>
                        <td>{b.dockerImage ? b.dockerImage : "-"}</td>
                    </tr>
                </table>

                <h3>Log</h3>
                <pre>{b.buildMessage}</pre>

                <h3>Bundle</h3>
                <pre>{b.patchBundle}</pre>
            </div>);
        }
    }
});

$(function() {
    /*
    * ROUTING
    */
    var renderComp = function(x) {
        React.renderComponent(x, document.getElementById('page-content'));
    }
    crossroads.addRoute('home', function () {
        renderComp(<FrozoneOverview />);
    });
    crossroads.addRoute('build/{build}', function (buildId) {
        renderComp(<FrozoneBuildDetails buildId={buildId} />);
    });

    //setup hasher
    function parseHash(newHash, oldHash){
        crossroads.parse(newHash);
    }
    function onHasherInit(curHash){
        if (curHash == '') {
            hasher.replaceHash('home');
        }
    }
    hasher.initialized.add(onHasherInit);
    hasher.initialized.add(parseHash);
    hasher.changed.add(parseHash);
    hasher.init();
});

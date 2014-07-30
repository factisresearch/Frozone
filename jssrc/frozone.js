/** @jsx React.DOM */

var FrozoneBuildBadge = React.createClass({
    render: function() {
        var b = this.props.build;
        var className = "label ";
        if (b.state == "failed" || b.state == "canceled" ) {
            className += "label-danger";
        } else if (b.state == "review-rejected" || b.state == "recheck") {
            className += "label-warning";
        } else if (b.state == "enqueued" || b.state == "preparing" || b.state == "started" || b.state == "in-review") {
            className += "label-info";
        } else if (b.state == "success" || b.state == "review-okay" || b.state == "applied") {
            className += "label-success";
        } else {
            className += "label-default";
        }
        return (<span className={className}>{b.state}</span>);
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
            canceled: false,
            patch: null
        }
    },

    componentDidMount: function() {
        this.fetchData();
    },

    fetchData: function() {
        $.ajax({
            url: "/api/patch/" + this.props.build.value.patch,
            dataType: 'json',
            success: function(data) {
                this.setState({ builds: data, patch: data });
            }.bind(this),
            error: function(xhr, status, err) {
                console.error("patch", status, err.toString());
            }.bind(this)
        });
    },

    render: function() {
        var build = this.props.build;

        var badge = <FrozoneBuildBadge build={build.value} />;

        return (<tr>
            <th>{build.key}</th>
            <td>{build.value.createdOn}</td>
            <td>{this.state.patch ? this.state.patch.name : "Loading..."}</td>
            <td>{badge}</td>
            <td>
                <a href={"#/build/" + build.key} title="More Information">
                    <i className="fa fa-info-circle"></i>
                </a>
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
                <th>Patch</th>
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
            filesChanged: [],
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

    getChangedFiles: function() {
        $.ajax({
            url: "/api/build/" + this.props.buildId + "/file-changes",
            dataType: 'json',
            success: function(data) {
                this.setState({ filesChanged: data });
            }.bind(this),
            error: function(xhr, status, err) {
                console.error("get-file-changes-" + this.props.buildId, status, err.toString());
                this.setState({ filesChanged: [] });
            }.bind(this)
        });
    },

    cancel: function(e) {
        e.preventDefault();
        var b = this.state.build;

        $.ajax({
            url: "/api/build/" + this.props.buildId + "/cancel",
            dataType: 'json',
            success: function(data) {
                this.fetchData();
            }.bind(this),
            error: function(xhr, status, err) {
                console.error("build-" + this.props.buildId + "-cancel", status, err.toString());
            }.bind(this)
        });
    },

    componentDidMount: function() {
        this.props.timer = setInterval(this.fetchData, 10000);
        this.fetchData();
        this.getChangedFiles();
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
            var canCancel = true;
            if(b.patchCanceledOn) {
                canCancel = false;
                cancelBox = (<div className="alert alert-danger" role="alert">
                    <strong>Patch was canceled: </strong>
                    <span>{b.patchCancelReason ? b.patchCancelReason : "Reason unknown"}</span>
                </div>);
            }

            var canReview = !!(b.buildSuccessOn);

            var filesChanged = this.state.filesChanged.map(function (change) {
                var label = <span className="label label-info">M</span>;
                if (!change.value.oldContents) {
                    label = <span className="label label-success">A</span>;
                }
                if (!change.value.newContents) {
                    label = <span className="label label-danger">R</span>;
                }

                return (<li key={change.key}>
                    {label} {change.value.filename}
                </li>);
            });

            return (<div>
            <div className="buildHeader clearFix">
                <h2 className="pull-left"><FrozoneBuildBadge build={b} /> Build #{this.props.buildId}</h2>

                <div className="buildButtons pull-right">
                    <button type="button" className="btn btn-danger" disabled={!canCancel} onClick={this.cancel}>Cancel patch</button>
                    <a className="btn btn-info" disabled={!canReview} href={"#/build/" + this.props.buildId +"/review"}>Review patch</a>
                    <button type="button" className="btn btn-success" disabled={true}>Apply patch</button>
                </div>
            </div>

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
                        <th>Image</th>
                        <td>{b.dockerImage ? b.dockerImage : "-"}</td>
                    </tr>
                </table>

                <h3>Files</h3>
                <ul className="list-unstyled">{filesChanged}</ul>

                <h3>Log</h3>
                <pre>{b.buildMessage}</pre>

                <h3>Bundle</h3>
                <pre>{b.patchBundle}</pre>
            </div>);
        }
    }
});

var FrozoneReview = React.createClass({
    getInitialState: function() {
        return {
            filesChanged: [],
            loaded: false
        }
    },

    getChangedFiles: function() {
        $.ajax({
            url: "/api/build/" + this.props.buildId + "/file-changes",
            dataType: 'json',
            success: function(data) {
                this.setState({ filesChanged: data, loaded: true });
            }.bind(this),
            error: function(xhr, status, err) {
                console.error("get-file-changes-" + this.props.buildId, status, err.toString());
                this.setState({ filesChanged: [] });
            }.bind(this)
        });
    },

    componentDidMount: function() {
        this.getChangedFiles();
    },

    render: function() {
        if (!this.state.loaded) {
            return (<FrozoneLoading />);
        }

        return (<div>
        <div className="buildHeader clearFix">
            <h2 className="pull-left">Review #{this.props.buildId}</h2>

            <div className="buildButtons pull-right">
                <button type="button" className="btn btn-danger" disabled={!canCancel} onClick={this.cancel}>Cancel patch</button>
                <a className="btn btn-info" disabled={!canReview} href={"#/build/" + this.props.buildId +"/review"}>Review patch</a>
                <button type="button" className="btn btn-success" disabled={true}>Apply patch</button>
            </div>
        </div>
        </div>);
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
    crossroads.addRoute('build/{build}/review', function (buildId) {
        renderComp(<FrozoneReview buildId={buildId} />);
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

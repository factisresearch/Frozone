/** @jsx React.DOM */
var FrozoneBuildList = React.createClass({
    render: function() {
        var rows = this.props.builds.map(function (build) {
            var buildStatus = <span className="label label-default">unknown</span>;
            if (build.value.buildEnqueuedOn) {
                buildStatus = <span className="label label-warning">enqueued</span>;
                if (build.value.buildStartedOn) {
                    buildStatus = <span className="label label-info">started</span>;
                    if (build.value.buildSuccess === true) {
                        buildStatus = <span className="label label-success">succeded</span>;
                    } else if (build.value.buildSuccess === false) {
                        buildStatus = <span className="label label-danger">failed</span>;
                    }
                }
            }

            return (<tr>
                <th>{build.key}</th>
                <td>{build.value.createdOn}</td>
                <td>{build.value.branch}</td>
                <td>{buildStatus}</td>
            </tr>);
        });

        return (<tbody>
            {rows}
        </tbody>);
    }
});

var FrozoneApp = React.createClass({
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
        setInterval(this.fetchData, 10000);
        this.fetchData();
    },

    render: function() {
        return (<table className="table table-condensed table-hover">
            <thead>
            <tr>
                <th>ID</th>
                <th>Created on</th>
                <th>Repository</th>
                <th>Status</th>
            </tr>
            </thead>
            <FrozoneBuildList builds={this.state.builds} />
        </table>);
    }
});

/*     branch T.Text
     path FilePath
     createdOn UTCTime
     notifyEmail [T.Text]
     changesHash T.Text
     patchBundle T.Text
     buildEnqueuedOn UTCTime Maybe
     buildStartedOn UTCTime Maybe
     buildSuccess Bool Maybe
     buildMessage T.Text Maybe
     dockerBaseImage T.Text Maybe
     dockerImage T.Text Maybe*/

$(function() {
    React.renderComponent(
      <FrozoneApp />,
      document.getElementById('page-content')
    );
});

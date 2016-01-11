;;; funcs.el --- Supporting functions for cb-csharp layer.
;;; Commentary:
;;; Code:

(require 'dash)
(require 's)

(autoload 'cb-buffers-current-line "cb-buffers")

(defun csharp/line-pad-usings ()
  "Add a trailing line of padding after usings."
  (save-excursion
    (goto-char (point-min))
    (when (search-forward-regexp (rx bol "using") nil t)
      (while (and (s-matches? (rx bol "using") (cb-buffers-current-line))
                  (not (eobp)))
        (forward-line))

      (unless (s-blank? (cb-buffers-current-line))
        (goto-char (line-beginning-position))
        (newline)))))

(defadvice omnisharp-fix-usings (after pad-usings activate)
  (csharp/line-pad-usings))


;;; Inserting namespaces

(defun csharp/do-insert-at-imports (ns)
  (save-excursion
    (goto-char (point-min))
    (insert (format "using %s;\n" ns))
    (csharp/line-pad-usings)))

(defun csharp/current-imported-namespaces ()
  "Return a list of namespaces imported in this buffer."
  (-map 'cadr (s-match-strings-all (rx (or bol (and ";" (* space)))
                                       "using" (+ space)
                                       (group (+ (any "." word))))
                                   (buffer-substring-no-properties (point-min) (point-max)))))

(defvar csharp/bcl-namespaces
  '("System" "System.Activities" "System.AddIn" "System.CodeDom" "System.ComponentModel" "System.Deployment"
    "System.Device.Location" "System.DirectoryServices" "System.Dynamic" "System.EnterpriseServices" "System.Globalization"
    "System.IdentityModel" "System.Management" "System.Media" "System.Messaging" "System.Numerics" "System.Printing"
    "System.Resources" "System.Runtime" "System.ServiceModel" "System.ServiceProcess" "System.Speech"
    "System.Timers" "System.Transactions" "System.Web" "System.Windows" "System.Workflow" "System.Xaml" "Accessibility"
    "Microsoft.Activities" "Microsoft.Build" "Microsoft.CSharp" "Microsoft.JScript" "Microsoft.SqlServer.Server" "Microsoft.VisualBasic" "Microsoft.VisualC" "Microsoft.Win32"
    "Microsoft.Windows" "UIAutomationClientsideProviders" "Windows.Foundation" "Windows.UI" "XamlGenerated"

    "System.Collections" "System.Collections.Concurrent" "System.Collections.Generic" "System.Collections.ObjectModel" "System.Collections.Specialized"

    "System.Configuration" "System.Configuration.Assemblies" "System.Configuration.Install" "System.Configuration.Internal" "System.Configuration.Provider"

    "System.Data" "System.Data.Common" "System.Data.Common.CommandTrees" "System.Data.Common.CommandTrees.ExpressionBuilder"
    "System.Data.Common.CommandTrees.ExpressionBuilder.Spatial" "System.Data.Common.EntitySql" "System.Data.Design" "System.Data.Entity.Design"
    "System.Data.Entity.Design.AspNet" "System.Data.Entity.Design.PluralizationServices" "System.Data.EntityClient" "System.Data.Linq" "System.Data.Linq.Mapping"
    "System.Data.Linq.SqlClient" "System.Data.Linq.SqlClient.Implementation" "System.Data.Mapping" "System.Data.Metadata.Edm" "System.Data.Objects"
    "System.Data.Objects.DataClasses" "System.Data.Objects.SqlClient" "System.Data.Odbc" "System.Data.OleDb" "System.Data.OracleClient" "System.Data.Services"
    "System.Data.Services.BuildProvider" "System.Data.Services.Client" "System.Data.Services.Common" "System.Data.Services.Configuration" "System.Data.Services.Design"
    "System.Data.Services.Internal" "System.Data.Services.Providers" "System.Data.Spatial" "System.Data.Sql" "System.Data.SqlClient" "System.Data.SqlTypes"

    "System.Diagnostics" "System.Diagnostics.CodeAnalysis" "System.Diagnostics.Contracts" "System.Diagnostics.Contracts.Internal" "System.Diagnostics.Design"
    "System.Diagnostics.Eventing" "System.Diagnostics.Eventing.Reader" "System.Diagnostics.PerformanceData" "System.Diagnostics.SymbolStore" "System.Diagnostics.Tracing"


    "System.Drawing" "System.Drawing.Configuration" "System.Drawing.Design" "System.Drawing.Drawing2D" "System.Drawing.Imaging" "System.Drawing.Printing" "System.Drawing.Text"

    "System.IO" "System.IO.Compression" "System.IO.IsolatedStorage" "System.IO.Log" "System.IO.MemoryMappedFiles" "System.IO.Packaging" "System.IO.Pipes" "System.IO.Ports"

    "System.Linq" "System.Linq.Expressions"

    "System.Net" "System.Net.Cache" "System.Net.Configuration" "System.Net.Http" "System.Net.Http.Headers" "System.Net.Mail" "System.Net.Mime" "System.Net.NetworkInformation"
    "System.Net.PeerToPeer" "System.Net.PeerToPeer.Collaboration" "System.Net.Security" "System.Net.Sockets" "System.Net.WebSockets"

    "System.Reflection" "System.Reflection.Context" "System.Reflection.Emit"

    "System.Runtime" "System.Runtime.CompilerServices" "System.Runtime.ConstrainedExecution" "System.Runtime.DesignerServices" "System.Runtime.DurableInstancing"
    "System.Runtime.ExceptionServices" "System.Runtime.Hosting" "System.Runtime.InteropServices" "System.Runtime.InteropServices.ComTypes"
    "System.Runtime.InteropServices.CustomMarshalers" "System.Runtime.InteropServices.Expando" "System.Runtime.InteropServices.WindowsRuntime" "System.Runtime.Remoting"
    "System.Runtime.Remoting.Activation" "System.Runtime.Remoting.Channels" "System.Runtime.Remoting.Channels.Http" "System.Runtime.Remoting.Channels.Ipc"
    "System.Runtime.Remoting.Channels.Tcp" "System.Runtime.Remoting.Contexts" "System.Runtime.Remoting.Lifetime" "System.Runtime.Remoting.Messaging"
    "System.Runtime.Remoting.Metadata" "System.Runtime.Remoting.Metadata.W3cXsd2001" "System.Runtime.Remoting.MetadataServices" "System.Runtime.Remoting.Proxies"
    "System.Runtime.Remoting.Services" "System.Runtime.Serialization" "System.Runtime.Serialization.Configuration" "System.Runtime.Serialization.Formatters"
    "System.Runtime.Serialization.Formatters.Binary" "System.Runtime.Serialization.Formatters.Soap" "System.Runtime.Serialization.Json" "System.Runtime.Versioning"

    "System.Security" "System.Security.AccessControl" "System.Security.Authentication" "System.Security.Authentication.ExtendedProtection"
    "System.Security.Authentication.ExtendedProtection.Configuration" "System.Security.Claims" "System.Security.Cryptography" "System.Security.Cryptography.Pkcs"
    "System.Security.Cryptography.X509Certificates" "System.Security.Cryptography.Xml" "System.Security.Permissions" "System.Security.Policy"
    "System.Security.Principal" "System.Security.RightsManagement"

    "System.ServiceModel" "System.ServiceModel.Activation" "System.ServiceModel.Activation.Configuration" "System.ServiceModel.Activities"
    "System.ServiceModel.Activities.Activation" "System.ServiceModel.Activities.Configuration" "System.ServiceModel.Activities.Description"
    "System.ServiceModel.Activities.Presentation" "System.ServiceModel.Activities.Presentation.Factories" "System.ServiceModel.Activities.Tracking"
    "System.ServiceModel.Activities.Tracking.Configuration" "System.ServiceModel.Channels" "System.ServiceModel.ComIntegration" "System.ServiceModel.Configuration"
    "System.ServiceModel.Description" "System.ServiceModel.Diagnostics" "System.ServiceModel.Discovery" "System.ServiceModel.Discovery.Configuration"
    "System.ServiceModel.Discovery.Version11" "System.ServiceModel.Discovery.VersionApril2005" "System.ServiceModel.Discovery.VersionCD1" "System.ServiceModel.Dispatcher"
    "System.ServiceModel.Internal" "System.ServiceModel.MsmqIntegration" "System.ServiceModel.PeerResolvers" "System.ServiceModel.Persistence" "System.ServiceModel.Routing"
    "System.ServiceModel.Routing.Configuration" "System.ServiceModel.Security" "System.ServiceModel.Security.Tokens" "System.ServiceModel.ServiceMoniker40"
    "System.ServiceModel.Syndication" "System.ServiceModel.Web" "System.ServiceModel.XamlIntegration"

    "System.Text" "System.Text.RegularExpressions"

    "System.Threading" "System.Threading.Tasks" "System.Threading.Tasks.Dataflow"

    "System.Web" "System.Web.ApplicationServices" "System.Web.Caching" "System.Web.ClientServices" "System.Web.ClientServices.Providers" "System.Web.Compilation"
    "System.Web.Configuration" "System.Web.Configuration.Internal" "System.Web.DynamicData" "System.Web.DynamicData.Design" "System.Web.DynamicData.ModelProviders"
    "System.Web.Handlers" "System.Web.Hosting" "System.Web.Instrumentation" "System.Web.Mail" "System.Web.Management" "System.Web.Mobile" "System.Web.ModelBinding"
    "System.Web.Profile" "System.Web.Query.Dynamic" "System.Web.RegularExpressions" "System.Web.Routing" "System.Web.Script" "System.Web.Script.Serialization"
    "System.Web.Script.Services" "System.Web.Security" "System.Web.Security.AntiXss" "System.Web.Services" "System.Web.Services.Configuration" "System.Web.Services.Description"
    "System.Web.Services.Discovery" "System.Web.Services.Protocols" "System.Web.SessionState" "System.Web.UI" "System.Web.UI.Adapters" "System.Web.UI.DataVisualization.Charting"
    "System.Web.UI.Design" "System.Web.UI.Design.Directives" "System.Web.UI.Design.MobileControls" "System.Web.UI.Design.MobileControls.Converters"
    "System.Web.UI.Design.WebControls" "System.Web.UI.Design.WebControls.WebParts" "System.Web.UI.HtmlControls" "System.Web.UI.MobileControls"
    "System.Web.UI.MobileControls.Adapters" "System.Web.UI.MobileControls.Adapters.XhtmlAdapters" "System.Web.UI.WebControls" "System.Web.UI.WebControls.Adapters"
    "System.Web.UI.WebControls.Expressions" "System.Web.UI.WebControls.WebParts" "System.Web.Util" "System.Web.WebSockets"

    "System.Xml" "System.Xml.Linq" "System.Xml.Resolvers" "System.Xml.Schema" "System.Xml.Serialization" "System.Xml.Serialization.Advanced"
    "System.Xml.Serialization.Configuration" "System.Xml.XmlConfiguration" "System.Xml.XPath" "System.Xml.Xsl" "System.Xml.Xsl.Runtime"
    ))

(defun csharp/insert-using (ns)
  (interactive
   (list
    (let ((cs (-difference csharp/bcl-namespaces
                           (csharp/current-imported-namespaces))))
      (completing-read "Namespace: " cs))))

  (if (s-matches? (rx-to-string `(and "using" (+ space) ,ns ";" (* space) eol))
                  (buffer-string))
      (when (called-interactively-p nil)
        (message "Already using '%s'" ns))
    (csharp/do-insert-at-imports ns)))

;;; funcs.el ends here

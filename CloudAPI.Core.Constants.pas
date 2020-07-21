unit CloudAPI.Core.Constants;

interface

type
  TcaConstException = class
  public const
    PARAMETER_REQIRED = 'Parameter "{Parameter.Name}" is requaired in method "{Method}"!';
    RAISED_AT_FORMAT = 'hh:mm:ss.zzz';
    EXCEPTION_MSG_FORMAT = '({RaisedAt}) [{Code}] {Message}';
  end;

implementation

end.

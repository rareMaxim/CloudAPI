unit CloudAPI.MethodLimits;

interface

type

  TcaMethodLimit = class
  private
    FTimeLimit: Integer;
    FIsGlobalLimit: Boolean;
    FMethodName: string;
  published
    property TimeLimit: Integer read FTimeLimit write FTimeLimit;
    property IsGlobalLimit: Boolean read FIsGlobalLimit write FIsGlobalLimit;
    property MethodName: string read FMethodName write FMethodName;
  end;

  TcaMethodLimitManager = class

  end;

implementation

end.

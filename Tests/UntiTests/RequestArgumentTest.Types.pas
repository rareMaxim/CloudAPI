unit RequestArgumentTest.Types;

interface

uses
  CloudAPI.Attributes,
  CloudAPI.Types;

type
  TOnePairRecord = record
  public
    [caName('key')]
    Value: string;
    class function GetValue: TOnePairRecord; static;
  end;

  TTwoPairRecord = record
  public
    [caName('key1')]
    Value1: string;
    [caName('key2')]
    Value2: string;
    class function GetValue: TTwoPairRecord; static;
  end;

  [caName('getMe')]
  [caParameterType(TcaParameterType.QueryString)]
  TOnePairClass = class
  private
    [caName('key')]
    fValue: string;
  public
    property Value: string read fValue write fValue;
    class function GetValue: TOnePairClass; static;
  end;

  TBaseClass = class
  private
    fExcludeParam: integer;
    [caName('key1')]
    fIncludeParam: integer;
  end;

  [caName('getMe')]
  [caParameterType(TcaParameterType.QueryString)]
  TTwoPairClass = class(TBaseClass)
  private
    [caName('key2')]
    fValue: string;
  public
    property Value: string read fValue write fValue;
    class function GetValue: TTwoPairClass; static;
  end;

implementation

{ TOnePairRecord }

class function TOnePairRecord.GetValue: TOnePairRecord;
begin
  Result.Value := 'Value';
end;

{ TTwoPairRecord }

class function TTwoPairRecord.GetValue: TTwoPairRecord;
begin
  Result.Value1 := 'Value1';
  Result.Value2 := 'Value2';
end;

{ TOnePairClass }

class function TOnePairClass.GetValue: TOnePairClass;
begin
  Result := TOnePairClass.Create;
  Result.fValue := 'Value';
end;

{ TTwoPairClass }

class function TTwoPairClass.GetValue: TTwoPairClass;
begin
  Result := TTwoPairClass.Create;
  Result.fValue := 'Value';
  Result.fIncludeParam := 1;
end;

end.

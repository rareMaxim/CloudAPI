unit CloudAPI.Attributes;

interface

uses
  CloudAPI.Types;

type
  TcaCustomAttribute = class(TCustomAttribute);

  caNameAttribute = class(TcaCustomAttribute)
  private
    FName: string;
  public
    constructor Create(const AName: string);
    property Name: string read FName write FName;
  end;

  caMethodAttribute = class(TcaCustomAttribute)
  private
    FMethod: TcaMethod;
  public
    constructor Create(const AMethod: TcaMethod);
    property Method: TcaMethod read FMethod write FMethod;
  end;

  caDefaultValueAttribute = class(TcaCustomAttribute)
  public
    function ToString: string; override; abstract;
  end;

  caDefaultValueAttribute<T> = class(caDefaultValueAttribute)
  private
    FDefaultValue: T;
  public
    constructor Create(const ADefaultValue: T);
    property DefaultValue: T read FDefaultValue write FDefaultValue;
  end;

  caDefaultValueStringAttribute = class(caDefaultValueAttribute<string>)
  public
    function ToString: string; override;
  end;

  caDefaultValueIntAttribute = class(caDefaultValueAttribute<Integer>)
  public
    function ToString: string; override;
  end;

  caDefaultValueInt64Attribute = class(caDefaultValueAttribute<Int64>)
  public
    function ToString: string; override;
  end;

  caDefaultValueSingleAttribute = class(caDefaultValueAttribute<Single>)
  public
    function ToString: string; override;
  end;

  caDefaultValueBooleanAttribute = class(caDefaultValueAttribute<Boolean>)
  public
    function ToString: string; override;
  end;

  caIsRequairedAttribute = class(TcaCustomAttribute)
  private
    FIsRequired: Boolean;
  public
    constructor Create(const AIsRequired: Boolean); overload;
    constructor Create; overload;
    property IsRequired: Boolean read FIsRequired write FIsRequired;
  end;

  caParameterTypeAttribute = class(TcaCustomAttribute)
  private
    FParameterType: TcaParameterType;
  public
    constructor Create(const AParameterType: TcaParameterType);
    property ParameterType: TcaParameterType read FParameterType write FParameterType;
  end;

  caLimitedMethodAttribute = class(TcaCustomAttribute)
  private
    FLimit: Int64;
    FIsGlobal: Boolean;
  public
    constructor Create(const ALimit: Int64; AIsGlobal: Boolean);
    property Limit: Int64 read FLimit write FLimit;
    property IsGlobal: Boolean read FIsGlobal write FIsGlobal;
  end;

implementation

uses
  System.SysUtils;

{ caNameAttribute }
constructor caNameAttribute.Create(const AName: string);
begin
  FName := AName;
end;

{ caDefaultValueAttribute }
constructor caDefaultValueAttribute<T>.Create(const ADefaultValue: T);
begin
  inherited Create();
  FDefaultValue := ADefaultValue;
end;

{ caDefaultValueStringAttribute }

function caDefaultValueStringAttribute.ToString: string;
begin
  Result := FDefaultValue;
end;

{ caDefaultValueInt64Attribute }

function caDefaultValueInt64Attribute.ToString: string;
begin
  Result := FDefaultValue.ToString;
end;

{ caIsRequairedAttribute }
constructor caIsRequairedAttribute.Create;
begin
  FIsRequired := True;
end;

constructor caIsRequairedAttribute.Create(const AIsRequired: Boolean);
begin
  FIsRequired := AIsRequired;
end;

{ caParameterTypeAttribute }

constructor caParameterTypeAttribute.Create(const AParameterType: TcaParameterType);
begin
  FParameterType := AParameterType;
end;

{ caDefaultValueBooleanAttribute }

function caDefaultValueBooleanAttribute.ToString: string;
begin
  Result := FDefaultValue.ToString(TUseBoolStrs.True);
end;

{ caMethodAttribute }

constructor caMethodAttribute.Create(const AMethod: TcaMethod);
begin
  FMethod := AMethod;
end;

{ caLimitedMethodAttribute }

constructor caLimitedMethodAttribute.Create(const ALimit: Int64; AIsGlobal: Boolean);
begin
  FLimit := ALimit;
  FIsGlobal := AIsGlobal;
end;

{ caDefaultValueSingleAttribute }

function caDefaultValueSingleAttribute.ToString: string;
begin
  Result := FDefaultValue.ToString;
end;

{ caDefaultValueIntAttribute }

function caDefaultValueIntAttribute.ToString: string;
begin
  Result := FDefaultValue.ToString;
end;

end.

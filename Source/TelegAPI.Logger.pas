unit TelegAPI.Logger;

interface

uses
  TelegAPI.Base,
  System.SysUtils;

type
  TLogLevel = (Unknown, Trace, Debug, Text, Info, Warn, Error, Fatal);

{$REGION 'ILogger'}

  ILogger = interface
    ['{0FAACA17-5BE8-4676-BD21-C010208C48D5}']
{$REGION 'Log'}
    procedure Log(level: TLogLevel; const msg: string); overload;
    procedure Log(level: TLogLevel; const msg: string; const e: Exception); overload;
    procedure Log(level: TLogLevel; const fmt: string; const args: array of
      const); overload;
    procedure Log(level: TLogLevel; const fmt: string; const args: array of
      const; const e: Exception); overload;
{$ENDREGION}
{$REGION 'Fatal'}
    procedure Fatal(const msg: string); overload;
    procedure Fatal(const msg: string; const e: Exception); overload;
    procedure Fatal(const fmt: string; const args: array of const); overload;
    procedure Fatal(const fmt: string; const args: array of const; const e:
      Exception); overload;
{$ENDREGION}
{$REGION 'Error'}
    procedure Error(const msg: string); overload;
    procedure Error(const msg: string; const e: Exception); overload;
    procedure Error(const fmt: string; const args: array of const); overload;
    procedure Error(const fmt: string; const args: array of const; const e:
      Exception); overload;
{$ENDREGION}
{$REGION 'Enter'}
    procedure Enter(const methodName: string); overload;
    procedure Enter(const instance: TObject; const methodName: string); overload;
{$ENDREGION}
{$REGION 'Leave'}
    procedure Leave(const methodName: string); overload;
    procedure Leave(const instance: TObject; const methodName: string); overload;
{$ENDREGION}
  end;

  TLogAbstract = class(TtgAbstractComponent, ILogger)
  public
{$REGION 'Log'}
    procedure Log(level: TLogLevel; const msg: string; const e: Exception);
      overload; virtual; abstract;
    procedure Log(level: TLogLevel; const msg: string); overload;
    procedure Log(level: TLogLevel; const fmt: string; const args: array of
      const); overload;
    procedure Log(level: TLogLevel; const fmt: string; const args: array of
      const; const e: Exception); overload;
{$ENDREGION}
{$REGION 'Fatal'}
    procedure Fatal(const msg: string); overload;
    procedure Fatal(const msg: string; const e: Exception); overload;
    procedure Fatal(const fmt: string; const args: array of const); overload;
    procedure Fatal(const fmt: string; const args: array of const; const e:
      Exception); overload;
{$ENDREGION}
{$REGION 'Error'}
    procedure Error(const msg: string); overload;
    procedure Error(const msg: string; const e: Exception); overload;
    procedure Error(const fmt: string; const args: array of const); overload;
    procedure Error(const fmt: string; const args: array of const; const e:
      Exception); overload;
{$ENDREGION}

{$REGION 'Enter'}
    procedure Enter(const methodName: string); overload;
    procedure Enter(const instance: TObject; const methodName: string); overload;
      virtual; abstract;
{$ENDREGION}
{$REGION 'Leave'}
    procedure Leave(const methodName: string); overload;
    procedure Leave(const instance: TObject; const methodName: string); overload;
      virtual; abstract;
{$ENDREGION}
  end;

  TLogEmpty = class(TLogAbstract)
  public
    procedure Log(level: TLogLevel; const msg: string; const e: Exception);
      overload; override;
    procedure Enter(const instance: TObject; const methodName: string); overload;
      override;
    procedure Leave(const instance: TObject; const methodName: string); overload;
      override;
  end;

implementation


{ TLogAbstract }
{$REGION 'Log'}

procedure TLogAbstract.Log(level: TLogLevel; const fmt: string; const args:
  array of const; const e: Exception);
begin
  Log(level, string.Format(fmt, args), e);
end;

procedure TLogAbstract.Log(level: TLogLevel; const fmt: string; const args:
  array of const);
begin
  Log(level, fmt, args, nil);
end;

procedure TLogAbstract.Log(level: TLogLevel; const msg: string);
begin
  Log(level, msg, nil);
end;
{$ENDREGION}

{$REGION 'Fatal'}

procedure TLogAbstract.Fatal(const msg: string);
begin
  Fatal(msg, nil);
end;

procedure TLogAbstract.Fatal(const msg: string; const e: Exception);
begin
  Log(TLogLevel.Fatal, msg, e);
end;

procedure TLogAbstract.Fatal(const fmt: string; const args: array of const);
begin
  Fatal(fmt, args, nil);
end;
{$ENDREGION}

{$REGION 'Error'}

procedure TLogAbstract.Error(const msg: string);
begin
  Error(msg, nil);
end;

procedure TLogAbstract.Error(const msg: string; const e: Exception);
begin
  Log(TLogLevel.Error, msg, e);
end;

procedure TLogAbstract.Error(const fmt: string; const args: array of const);
begin
  Error(fmt, args, nil);
end;

procedure TLogAbstract.Error(const fmt: string; const args: array of const;
  const e: Exception);
begin
  Error(string.Format(fmt, args), nil);
end;
{$ENDREGION}


{$REGION 'Fatal'}

procedure TLogAbstract.Fatal(const fmt: string; const args: array of const;
  const e: Exception);
begin
  Fatal(string.Format(fmt, args), nil);
end;
{$ENDREGION}

{$REGION 'Leave'}

procedure TLogAbstract.Enter(const methodName: string);
begin
  Enter(nil, methodName);
end;
{$ENDREGION}

{$REGION 'Leave'}

procedure TLogAbstract.Leave(const methodName: string);
begin
  Leave(nil, methodName);
end;
{$ENDREGION}

{ TLogEmpty }

procedure TLogEmpty.Enter(const instance: TObject; const methodName: string);
begin

end;

procedure TLogEmpty.Leave(const instance: TObject; const methodName: string);
begin

end;

procedure TLogEmpty.Log(level: TLogLevel; const msg: string; const e: Exception);
begin
  inherited;
// nothing
end;

end.


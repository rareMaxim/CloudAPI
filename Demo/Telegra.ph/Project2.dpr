program Project2;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  TelegraPh,
  TelegAPI.Utils.Json,
  CrossUrl.SystemNet.HttpClient,
  System.SysUtils,
  TelegraPh.Types,
  System.Generics.Collections;

procedure createAccount;
var
  Tph: TTelegraPh;
  LTest: IphAccount;
begin
  Tph := TTelegraPh.Create(nil);
  Tph.HttpCore := TcuHttpClientSysNet.Create(nil);
  try
    LTest := Tph.createAccount('Sandbox', 'Anonymous');
    Writeln(LTest.short_name);
    Writeln(LTest.author_name);
    Writeln(LTest.author_url);
    Writeln(LTest.access_token);
    Writeln(LTest.auth_url);
    Writeln(LTest.page_count);
  finally
    Tph.Free;

  end;
end;

procedure createPage;
var
  Tph: TTelegraPh;
  LTest: IphPage;
  Content: TArray<IphNode>;
begin
  Tph := TTelegraPh.Create(nil);
  Tph.HttpCore := TcuHttpClientSysNet.Create(nil);
  try
    Content := [TphNode.Create('{"tag":"p","children":["Hello, world!"]}')];
    Writeln((Content[0] as TBaseJson).AsJson);
    LTest := Tph.createPage('b968da509bb76866c35425099bc0989a5ec3b32997d55286c657e6994bbb',
      'Sample Page', 'Anonymous', '', Content, True);
    Writeln((LTest as TBaseJson).AsJson);
  finally
    Tph.Free;
  end;
end;

begin
  try
    { TODO -oUser -cConsole Main : Insert code here }
    createPage;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.


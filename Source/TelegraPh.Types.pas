unit TelegraPh.Types;

interface

uses
  TelegAPI.Utils.Json;

type
{$REGION 'Account'}
  IphAccount = interface
    ['{AA9FE190-6D8B-44C4-9BC0-4A1BCECDFDD5}']
    function short_name: string;
    function author_name: string;
    function author_url: string;
    function access_token: string;
    function auth_url: string;
    function page_count: Integer;
  end;

  TphAccount = class(TBaseJson, IphAccount)
  public
    function short_name: string;
    function author_name: string;
    function author_url: string;
    function access_token: string;
    function auth_url: string;
    function page_count: Integer;
  end;
{$ENDREGION}

{$REGION 'Node'}

  IphNode = interface;

  TphContent = TArray<IphNode>;

  IphContentHelper = record helper for TphContent
    function ToJson: string;
  end;

  IphNode = interface
    ['{BC48D648-0A3D-427D-9D3E-E1765DF70932}']
    function Value: string;
    function Tag: string;
    function attrs: string;
    function children: TphContent;
  end;

  TphNode = class(TBaseJson, IphNode)
  public
    function Value: string;
    function Tag: string;
    function attrs: string;
    function children: TphContent;
    constructor Create(const ATag, AAttrs: string; AChildren: TphContent); overload;
    constructor Create(const AValue: string); overload;
  end;
{$ENDREGION}

{$REGION 'Page'}

  IphPage = interface
    ['{80BB10CE-9452-4F2E-914C-BEED76A98DAF}']
    function path: string;
    function url: string;
    function title: string;
    function description: string;
    function author_name: string;
    function author_url: string;
    function image_url: string;
    function content: TphContent;
    function views: Integer;
    function can_edit: Boolean;
  end;

  TphPage = class(TBaseJson, IphPage)
  public
    function path: string;
    function url: string;
    function title: string;
    function description: string;
    function author_name: string;
    function author_url: string;
    function image_url: string;
    function content: TphContent;
    function views: Integer;
    function can_edit: Boolean;
  end;
{$ENDREGION}

{$REGION 'PageList'}

  IphPageList = interface
    ['{551C6999-1C7D-4CE6-B44A-25F174A881DF}']
    function total_count: Integer;
    function pages: TphContent;
  end;

  TphPageList = class(TBaseJson, IphPageList)
  public
    function total_count: Integer;
    function pages: TphContent;
  end;
{$ENDREGION}
{$REGION 'PageViews'}

  IphPageViews = interface
    ['{EACCDD18-450B-42F6-BF5B-7FACE18D912E}']
    function views: Integer;
  end;

  TphPageViews = class(TBaseJson, IphPageViews)
  public
    function views: Integer;
  end;
{$ENDREGION}

implementation
{$REGION 'Account'}
{ TphAccount }

function TphAccount.access_token: string;
begin
  Result := ReadToSimpleType<string>('access_token');
end;

function TphAccount.author_name: string;
begin
  Result := ReadToSimpleType<string>('author_name');
end;

function TphAccount.author_url: string;
begin
  Result := ReadToSimpleType<string>('author_url');
end;

function TphAccount.auth_url: string;
begin
  Result := ReadToSimpleType<string>('auth_url');
end;

function TphAccount.page_count: Integer;
begin
  Result := ReadToSimpleType<Integer>('page_count');
end;

function TphAccount.short_name: string;
begin
  Result := ReadToSimpleType<string>('short_name');
end;
{$ENDREGION}
{$REGION 'Node'}
{ TphNode }

function TphNode.attrs: string;
begin
  Result := ReadToSimpleType<string>('attrs');
end;

function TphNode.children: TphContent;
begin
  Result := ReadToArray<IphNode>(TphNode, 'children');
end;

constructor TphNode.Create(const AValue: string);
begin
  inherited;
  SetJson(AValue);
end;

constructor TphNode.Create(const ATag, AAttrs: string; AChildren: TphContent);
begin
  inherited Create('');
  FJSON.S['tag'] := ATag;
  FJSON.S['attrs'] := AAttrs;
  FJSON.S['children'] := AChildren.ToJson;
end;

function TphNode.Tag: string;
begin
  Result := ReadToSimpleType<string>('tag');
end;

function TphNode.Value: string;
begin
  Result := FJSON.ToJSON;
end;
{ IphContentHelper }

function IphContentHelper.ToJson: string;
var
  I: Integer;
begin
  Result := '[';
  for I := Low(Self) to High(Self) do
    if Assigned(Self[I]) then
    begin
      Result := Result + (Self[I] as TBaseJson).AsJson;
      if I <> High(Self) then
        Result := Result + ',';
    end;
  Result := Result + ']';
end;
{$ENDREGION}
{$REGION 'Page'}

{ TphPage }

function TphPage.author_name: string;
begin
  Result := ReadToSimpleType<string>('author_name');
end;

function TphPage.author_url: string;
begin
  Result := ReadToSimpleType<string>('author_url');
end;

function TphPage.can_edit: Boolean;
begin
  Result := ReadToSimpleType<Boolean>('can_edit');
end;

function TphPage.content: TArray<IphNode>;
begin
  Result := ReadToArray<IphNode>(TphPage, 'content')
end;

function TphPage.description: string;
begin
  Result := ReadToSimpleType<string>('description');
end;

function TphPage.image_url: string;
begin
  Result := ReadToSimpleType<string>('image_url');
end;

function TphPage.path: string;
begin
  Result := ReadToSimpleType<string>('path');
end;

function TphPage.title: string;
begin
  Result := ReadToSimpleType<string>('title');
end;

function TphPage.url: string;
begin
  Result := ReadToSimpleType<string>('url');
end;

function TphPage.views: Integer;
begin
  Result := ReadToSimpleType<Integer>('views');
end;
{$ENDREGION}

{$REGION 'PageList'}
{ TphPageList }

function TphPageList.pages: TphContent;
begin
  Result := ReadToArray<IphNode>(TphNode, 'pages');
end;

function TphPageList.total_count: Integer;
begin
  Result := ReadToSimpleType<Integer>('total_count');
end;
{$ENDREGION}

{$REGION 'PageViews'}
{ TphPageViews }

function TphPageViews.views: Integer;
begin
  Result := ReadToSimpleType<Integer>('views');
end;
{$ENDREGION}

end.


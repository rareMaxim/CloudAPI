unit TelegraPh.Types;

interface

uses
  CloudAPI.Utils.Json;

type
{$REGION 'Account'}
  /// <summary>
  ///   This object represents a Telegraph account.
  /// </summary>
  IphAccount = interface
    ['{AA9FE190-6D8B-44C4-9BC0-4A1BCECDFDD5}']
    /// <summary>
    ///   Account name, helps users with several accounts remember which they
    ///   are currently using. Displayed to the user above the "Edit/Publish"
    ///   button on Telegra.ph, other users don't see this name.
    /// </summary>
    function short_name: string;
    /// <summary>
    ///   Default author name used when creating new articles.
    /// </summary>
    function author_name: string;
    /// <summary>
    ///   Profile link, opened when users click on the author's name below the
    ///   title. Can be any link, not necessarily to a Telegram profile or
    ///   channel.
    /// </summary>
    function author_url: string;
    /// <summary>
    ///   Optional. Only returned by the createAccount and revokeAccessToken
    ///   method. Access token of the Telegraph account.
    /// </summary>
    function access_token: string;
    /// <summary>
    ///   Optional. URL to authorize a browser on telegra.ph and connect it to
    ///   a Telegraph account. This URL is valid for only one use and for 5
    ///   minutes only.
    /// </summary>
    function auth_url: string;
    /// <summary>
    ///   Optional. Number of pages belonging to the Telegraph account. <br />
    /// </summary>
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

  /// <summary>
  ///   This abstract object represents a DOM Node. It can be a String which
  ///   represents a DOM text node or a NodeElement object.
  /// </summary>
  IphNode = interface
    ['{BC48D648-0A3D-427D-9D3E-E1765DF70932}']
    function Value: string;
    /// <summary>
    ///   Name of the DOM element. Available tags: a, aside, b, blockquote, br,
    ///   code, em, figcaption, figure, h3, h4, hr, i, iframe, img, li, ol, p,
    ///   pre, s, strong, u, ul, video.
    /// </summary>
    function Tag: string;
    /// <summary>
    ///   Optional. Attributes of the DOM element. Key of object represents
    ///   name of attribute, value represents value of attribute. Available
    ///   attributes: href, src.
    /// </summary>
    function attrs: string;
    /// <summary>
    ///   Optional. List of child nodes for the DOM element.
    /// </summary>
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

  /// <summary>
  ///   This object represents a page on Telegraph.
  /// </summary>
  IphPage = interface
    ['{80BB10CE-9452-4F2E-914C-BEED76A98DAF}']
    /// <summary>
    ///   Path to the page.
    /// </summary>
    function path: string;
    /// <summary>
    ///   URL of the page.
    /// </summary>
    function url: string;
    /// <summary>
    ///   Title of the page.
    /// </summary>
    function title: string;
    /// <summary>
    ///   Description of the page.
    /// </summary>
    function description: string;
    /// <summary>
    ///   Optional. Name of the author, displayed below the title.
    /// </summary>
    function author_name: string;
    /// <summary>
    ///   Optional. Profile link, opened when users click on the author's name
    ///   below the title. Can be any link, not necessarily to a Telegram
    ///   profile or channel.
    /// </summary>
    function author_url: string;
    /// <summary>
    ///   Optional. Image URL of the page.
    /// </summary>
    function image_url: string;
    /// <summary>
    ///   Optional. Content of the page. <br />
    /// </summary>
    function content: TphContent;
    /// <summary>
    ///   Number of page views for the page.
    /// </summary>
    function views: Integer;
    /// <summary>
    ///   Optional. Only returned if access_token passed. True, if the target
    ///   Telegraph account can edit the page.
    /// </summary>
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

  /// <summary>
  ///   This object represents a list of Telegraph articles belonging to an
  ///   account. Most recently created articles first.
  /// </summary>
  IphPageList = interface
    ['{551C6999-1C7D-4CE6-B44A-25F174A881DF}']
    /// <summary>
    ///   Total number of pages belonging to the target Telegraph account.
    /// </summary>
    function total_count: Integer;
    /// <summary>
    ///   Requested pages of the target Telegraph account.
    /// </summary>
    function pages: TphContent;
  end;

  TphPageList = class(TBaseJson, IphPageList)
  public
    function total_count: Integer;
    function pages: TphContent;
  end;
{$ENDREGION}

{$REGION 'PageViews'}

  /// <summary>
  ///   This object represents the number of page views for a Telegraph
  ///   article.
  /// </summary>
  IphPageViews = interface
    ['{EACCDD18-450B-42F6-BF5B-7FACE18D912E}']
    /// <summary>
    ///   Number of page views for the target page.
    /// </summary>
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
  Result := ToSimpleType<string>('access_token');
end;

function TphAccount.author_name: string;
begin
  Result := ToSimpleType<string>('author_name');
end;

function TphAccount.author_url: string;
begin
  Result := ToSimpleType<string>('author_url');
end;

function TphAccount.auth_url: string;
begin
  Result := ToSimpleType<string>('auth_url');
end;

function TphAccount.page_count: Integer;
begin
  Result := ToSimpleType<Integer>('page_count');
end;

function TphAccount.short_name: string;
begin
  Result := ToSimpleType<string>('short_name');
end;
{$ENDREGION}

{$REGION 'Node'}
{ TphNode }

function TphNode.attrs: string;
begin
  Result := ToSimpleType<string>('attrs');
end;

function TphNode.children: TphContent;
begin
  Result := ToArray<IphNode>(TphNode, 'children');
end;

constructor TphNode.Create(const AValue: string);
begin
  inherited;
  SetJson(AValue);
end;

constructor TphNode.Create(const ATag, AAttrs: string; AChildren: TphContent);
begin
  inherited Create('');
  GetJson.S['tag'] := ATag;
  GetJson.S['attrs'] := AAttrs;
  GetJson.S['children'] := AChildren.ToJson;
end;

function TphNode.Tag: string;
begin
  Result := ToSimpleType<string>('tag');
end;

function TphNode.Value: string;
begin
  Result := GetJson.ToJSON;
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
  Result := ToSimpleType<string>('author_name');
end;

function TphPage.author_url: string;
begin
  Result := ToSimpleType<string>('author_url');
end;

function TphPage.can_edit: Boolean;
begin
  Result := ToSimpleType<Boolean>('can_edit');
end;

function TphPage.content: TArray<IphNode>;
begin
  Result := ToArray<IphNode>(TphPage, 'content')
end;

function TphPage.description: string;
begin
  Result := ToSimpleType<string>('description');
end;

function TphPage.image_url: string;
begin
  Result := ToSimpleType<string>('image_url');
end;

function TphPage.path: string;
begin
  Result := ToSimpleType<string>('path');
end;

function TphPage.title: string;
begin
  Result := ToSimpleType<string>('title');
end;

function TphPage.url: string;
begin
  Result := ToSimpleType<string>('url');
end;

function TphPage.views: Integer;
begin
  Result := ToSimpleType<Integer>('views');
end;
{$ENDREGION}

{$REGION 'PageList'}
{ TphPageList }

function TphPageList.pages: TphContent;
begin
  Result := ToArray<IphNode>(TphNode, 'pages');
end;

function TphPageList.total_count: Integer;
begin
  Result := ToSimpleType<Integer>('total_count');
end;
{$ENDREGION}

{$REGION 'PageViews'}
{ TphPageViews }

function TphPageViews.views: Integer;
begin
  Result := ToSimpleType<Integer>('views');
end;
{$ENDREGION}

end.


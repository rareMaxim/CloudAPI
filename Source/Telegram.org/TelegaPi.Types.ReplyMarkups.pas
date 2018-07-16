unit TelegaPi.Types.ReplyMarkups;

interface

uses
  REST.Json.Types;

type
  TtgButtonBase = class
  private
    [JSONName('text')]
    FText: string;
  public
    property Text: string read FText write FText;
  end;

  TtgInlineKeyboardButton = class(TtgButtonBase)
  private
    [JSONName('callback_data')]
    FCallbackData: string;
    [JSONName('pay')]
    FPay: Boolean;
    [JSONName('url')]
    FURL: string;
    [JSONName('switch_inline_query')]
    FSwitchInlineQuery: string;
    [JSONName('switch_inline_query_current_chat')]
    FSwitchInlineQueryCurrentChat: string;
    [JSONName('callback_game')]
    FCallbackGame: string;
  public
    constructor Create(const AText: string); overload;
    constructor Create(const AText, ACallbackData: string); overload;
    property Url: string read FURL write FURL;
    property CallbackData: string read FCallbackData write FCallbackData;
    property SwitchInlineQuery: string read FSwitchInlineQuery write FSwitchInlineQuery;
    property SwitchInlineQueryCurrentChat: string read
      FSwitchInlineQueryCurrentChat write FSwitchInlineQueryCurrentChat;
    property CallbackGame: string read FCallbackGame write FCallbackGame;
    property Pay: Boolean read FPay write FPay;
  end;

  TtgKeyboardButton = class(TtgButtonBase)
  private
    [JSONName('request_location')]
    FRequestLocation: Boolean;
    [JSONName('request_contact')]
    FRequestContact: Boolean;
  public
    constructor Create(const AText: string; ARequestContact: Boolean = False;
      ARequestLocation: Boolean = False); overload;
    property RequestContact: Boolean read FRequestContact write FRequestContact;
    property RequestLocation: Boolean read FRequestLocation write FRequestLocation;
  end;

  IReplyMarkup = interface
    ['{4DCF23BA-8A37-46EF-A832-F325532B509A}']
  end;

  TtgReplyMarkup = class abstract(TInterfacedObject, IReplyMarkup)
  private
    [JSONName('selective')]
    FSelective: Boolean;
  public
    property Selective: Boolean read FSelective write FSelective;
  end;

  TtgForceReply = class(TtgReplyMarkup)
  private
    [JSONName('force_reply')]
    FForce: Boolean;
  public
    property Force: Boolean read FForce write FForce;
  end;

  TtgInlineKeyboardMarkup = class(TInterfacedObject, IReplyMarkup)
  private
    [JSONName('inline_keyboard')]
    FKeyboard: TArray<TArray<TtgInlineKeyboardButton>>;
  public
    procedure AddRow(AKeyboardRow: TArray<TtgInlineKeyboardButton>);
    constructor Create; overload;
    constructor Create(AInlineKeyboardRow: TArray<TtgInlineKeyboardButton>); overload;
    constructor Create(AInlineKeyboard: TArray<TArray<TtgInlineKeyboardButton>>);
      overload;
    destructor Destroy; override;
    property Keyboard: TArray<TArray<TtgInlineKeyboardButton>> read FKeyboard
      write FKeyboard;
  end;

  TtgReplyKeyboardMarkup = class(TtgReplyMarkup)
  private
    [JSONName('resize_keyboard')]
    FResizeKeyboard: Boolean;
    [JSONName('one_time_keyboard')]
    FOneTimeKeyboard: Boolean;
    [JSONName('selective')]
    FSelective: Boolean;
    [JSONName('keyboard')]
    FKeyboard: TArray<TArray<TtgKeyboardButton>>;
  public
    procedure AddRow(AKeyboardRow: TArray<TtgKeyboardButton>);
    constructor Create(AResizeKeyboard, AOneTimeKeyboard: Boolean); overload;
    constructor Create(AKeyboardRow: TArray<TtgKeyboardButton>; AResizeKeyboard:
      Boolean = False; AOneTimeKeyboard: Boolean = False); overload;
    constructor Create(AKeyboard: TArray<TArray<TtgKeyboardButton>>;
      AResizeKeyboard: Boolean = False; AOneTimeKeyboard: Boolean = False); overload;
    destructor Destroy; override;
    property Keyboard: TArray<TArray<TtgKeyboardButton>> read FKeyboard write FKeyboard;
    property OneTimeKeyboard: Boolean read FOneTimeKeyboard write FOneTimeKeyboard;
    property ResizeKeyboard: Boolean read FResizeKeyboard write FResizeKeyboard;
    property Selective: Boolean read FSelective write FSelective;
  end;

  TtgReplyKeyboardRemove = class(TtgReplyMarkup)
  private
    [JSONName('remove_keyboard')]
    FRemoveKeyboard: Boolean;
  public
    constructor Create(ARemoveKeyboard: Boolean = True);
    property RemoveKeyboard: Boolean read FRemoveKeyboard write FRemoveKeyboard;
  end;

implementation



constructor TtgInlineKeyboardButton.Create(const AText: string);
begin
  Text := AText;
end;

constructor TtgInlineKeyboardButton.Create(const AText, ACallbackData: string);
begin
  Self.Create(AText);
  Self.CallbackData := ACallbackData;
end;

constructor TtgKeyboardButton.Create(const AText: string; ARequestContact,
  ARequestLocation: Boolean);
begin
  inherited Create;
  Self.Text := AText;
  Self.RequestContact := ARequestContact;
  Self.RequestLocation := ARequestLocation;
end;

constructor TtgInlineKeyboardMarkup.Create(AInlineKeyboardRow: TArray<
  TtgInlineKeyboardButton>);
begin
  inherited Create;
  AddRow(AInlineKeyboardRow);
end;

procedure TtgInlineKeyboardMarkup.AddRow(AKeyboardRow: TArray<TtgInlineKeyboardButton>);
begin
  SetLength(FKeyboard, Length(FKeyboard) + 1);
  FKeyboard[High(FKeyboard)] := AKeyboardRow;
end;

constructor TtgInlineKeyboardMarkup.Create(AInlineKeyboard: TArray<TArray<
  TtgInlineKeyboardButton>>);
var
  i: Integer;
begin
  Self.Create;
  for i := Low(AInlineKeyboard) to High(AInlineKeyboard) do
    AddRow(AInlineKeyboard[i]);
end;

destructor TtgInlineKeyboardMarkup.Destroy;
var
  i, j: Integer;
begin
  for i := Low(FKeyboard) to High(FKeyboard) do
    for j := Low(FKeyboard[i]) to High(FKeyboard[i]) do
      FKeyboard[i, j].Free;
  inherited;
end;

constructor TtgInlineKeyboardMarkup.Create;
begin
  inherited Create;
end;

constructor TtgReplyKeyboardMarkup.Create(AKeyboardRow: TArray<TtgKeyboardButton
  >; AResizeKeyboard, AOneTimeKeyboard: Boolean);
begin
  inherited Create;
  AddRow(AKeyboardRow);
  ResizeKeyboard := AResizeKeyboard;
  OneTimeKeyboard := AOneTimeKeyboard;
end;

procedure TtgReplyKeyboardMarkup.AddRow(AKeyboardRow: TArray<TtgKeyboardButton>);
begin
  SetLength(FKeyboard, Length(FKeyboard) + 1);
  FKeyboard[High(FKeyboard)] := AKeyboardRow;
end;

constructor TtgReplyKeyboardMarkup.Create(AKeyboard: TArray<TArray<
  TtgKeyboardButton>>; AResizeKeyboard, AOneTimeKeyboard: Boolean);
begin
  inherited Create;
  Keyboard := AKeyboard;
  ResizeKeyboard := AResizeKeyboard;
  OneTimeKeyboard := AOneTimeKeyboard;
end;

destructor TtgReplyKeyboardMarkup.Destroy;
var
  i, j: Integer;
begin
  for i := Low(FKeyboard) to High(FKeyboard) do
    for j := Low(FKeyboard[i]) to High(FKeyboard[i]) do
      FKeyboard[i, j].Free;
  inherited;
end;

constructor TtgReplyKeyboardMarkup.Create(AResizeKeyboard, AOneTimeKeyboard: Boolean);
begin
  inherited Create;
  ResizeKeyboard := AResizeKeyboard;
  OneTimeKeyboard := AOneTimeKeyboard;
end;

constructor TtgReplyKeyboardRemove.Create(ARemoveKeyboard: Boolean);
begin
  inherited Create;
  RemoveKeyboard := ARemoveKeyboard;
end;

end.


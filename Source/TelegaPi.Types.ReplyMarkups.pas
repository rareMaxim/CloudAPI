unit TelegaPi.Types.ReplyMarkups;

interface

uses
  XSuperObject,
  System.Generics.Collections,
  TelegaPi.Types;

type
  /// <summary>
  /// Objects implementing this Interface define how a <see cref="User"/> can reply to the sent <see cref="Message"/>
  /// </summary>
  IReplyMarkup = interface
    ['{4DCF23BA-8A37-46EF-A832-F325532B509A}']
  end;

  /// <summary>
  /// Defines how clients display a reply intreface to the user
  /// </summary>
  /// <seealso cref="Telegram.Bot.Types.ReplyMarkups.IReplyMarkup" />
  TtgReplyMarkup = class abstract(TInterfacedObject, IReplyMarkup)
  public
    /// <summary>
    /// Optional. Use this parameter if you want to show the keyboard to specific users only. Targets: 1) users that are @mentioned in the text of the Message object; 2) if the bot's message is a reply (has <see cref="Message.ReplyToMessage"/>), sender of the original message.
    /// Example: A user requests to change the bot's language, bot replies to the request with a keyboard to select the new language. Other users in the group don't see the keyboard.
    /// </summary>
    [Alias('selective')]
    Selective: Boolean;
    [DISABLE]
    property RefCount;
  end;

  /// <summary>
  /// Upon receiving a <see cref="Message"/> with this object, Telegram clients will display a reply interface to the user (act as if the user has selected the bot's message and tapped 'Reply'). This can be extremely useful if you want to create user-friendly step-by-step interfaces without having to sacrifice privacy mode.
  /// </summary>
  [Alias('ForceReply')]
  TtgForceReply = class(TtgReplyMarkup)
  public
    /// <summary>
    /// Shows reply interface to the user, as if they manually selected the
    /// bot‘s message and tapped ’Reply'
    /// </summary>
    [Alias('force_reply')]
    Force: Boolean;
  end;

  TtgButtonedMarkup<T: class> = class(TInterfacedObject, IReplyMarkup)
  private
    FKeyboard: TObjectList<TObjectList<T>>;
    constructor Create; overload;
    function GetKeyboard: TArray<TArray<T>>;
    procedure SetKeyboard(Value: TArray<TArray<T>>);
    /// <summary>
    /// Initializes a new instance of the <see cref="InlineKeyboardMarkup"/> class.
    /// </summary>
    /// <param name="inlineKeyboardRow">The inline keyboard row.</param>
    constructor Create(AKeyboardRow: TArray<T>); overload;
    /// <summary>
    /// Initializes a new instance of the <see cref="InlineKeyboardMarkup"/> class.
    /// </summary>
    /// <param name="inlineKeyboard">The inline keyboard.</param>
    constructor Create(AKeyboard: TArray<TArray<T>>); overload;
  public
    procedure AddRow(AKeyboardRow: TArray<T>);
    destructor Destroy; override;
        /// <summary>
    /// Array of <see cref="InlineKeyboardButton"/> rows, each represented by an Array of <see cref="InlineKeyboardButton"/>.
    /// </summary>
    property Keyboard: TArray<TArray<T>> read GetKeyboard write SetKeyboard;
  published
    [DISABLE]
    property RefCount;
  end;

  TtgButtonedReplyMarkup<T: class> = class(TtgButtonedMarkup<T>)
    /// <summary>
    /// Optional. Use this parameter if you want to show the keyboard to specific users only. Targets: 1) users that are @mentioned in the text of the Message object; 2) if the bot's message is a reply (has <see cref="Message.ReplyToMessage"/>), sender of the original message.
    /// Example: A user requests to change the bot's language, bot replies to the request with a keyboard to select the new language. Other users in the group don't see the keyboard.
    /// </summary>
    [Alias('selective')]
    Selective: Boolean;
  end;
  /// <summary>
  /// This object represents an inline keyboard that appears right next to the <see cref="Message"/> it belongs to.
  /// </summary>
  /// <remarks>
  /// Inline keyboards are currently being tested and are not available in channels yet. For now, feel free to use them in one-on-one chats or groups.
  /// </remarks>

  [Alias('InlineKeyboardMarkup')]
  TtgInlineKeyboardMarkup = class(TtgButtonedMarkup<TtgInlineKeyboardButton>)
  private
  public
    /// <summary>
    /// Initializes a new instance of the <see cref="InlineKeyboardMarkup"/> class.
    /// </summary>
    /// <param name="inlineKeyboardRow">The inline keyboard row.</param>
    constructor Create(AInlineKeyboardRow: TArray<TtgInlineKeyboardButton>); overload;
    /// <summary>
    /// Initializes a new instance of the <see cref="InlineKeyboardMarkup"/> class.
    /// </summary>
    /// <param name="inlineKeyboard">The inline keyboard.</param>
    constructor Create(AInlineKeyboard: TArray<TArray<TtgInlineKeyboardButton>>); overload;
    procedure AddRow(AInlineKeyboardRow: TArray<TtgInlineKeyboardButton>);
    destructor Destroy; override;
        /// <summary>
    /// Array of <see cref="InlineKeyboardButton"/> rows, each represented by an Array of <see cref="InlineKeyboardButton"/>.
    /// </summary>
    [Alias('inline_keyboard')]
    property InlineKeyboard: TArray<TArray<TtgInlineKeyboardButton>> read GetKeyboard write SetKeyboard;
  published
    [DISABLE]
    property RefCount;
  end;

  /// <summary>
  /// This object represents a custom keyboard with reply options (see Introduction to bots for details and examples).
  /// </summary>
  [Alias('ReplyKeyboardMarkup')]
  TtgReplyKeyboardMarkup = class(TtgReplyMarkup)
  private
    FKeyboard: TObjectList<TObjectList<TtgKeyboardButton>>;
    constructor Create; overload;
    function GetKeyboard: TArray<TArray<TtgKeyboardButton>>;
    procedure SetKeyboard(Value: TArray<TArray<TtgKeyboardButton>>);
  public
    /// <summary>
    /// Optional. Requests clients to resize the keyboard vertically for optimal fit (e.g., make the keyboard smaller if there are just two rows of <see cref="KeyboardButton"/>). Defaults to <c>false</c>, in which case the custom keyboard is always of the same height as the app's standard keyboard.
    /// </summary>
    [Alias('resize_keyboard')]
    ResizeKeyboard: Boolean;
    /// <summary>
    /// Optional. Requests clients to hide the keyboard as soon as it's been used. Defaults to <c>false</c>.
    /// </summary>
    [Alias('one_time_keyboard')]
    OneTimeKeyboard: Boolean;

     /// <summary>
    /// Initializes a new instance of the <see cref="ReplyKeyboardMarkup"/> class.
    /// </summary>
    /// <param name="keyboardRow">The keyboard row.</param>
    /// <param name="resizeKeyboard">if set to <c>true</c> the keyboard resizes vertically for optimal fit.</param>
    /// <param name="oneTimeKeyboard">if set to <c>true</c> the client hides the keyboard as soon as it's been used.</param>
    constructor Create(AKeyboardRow: TArray<TtgKeyboardButton>; AResizeKeyboard: Boolean = False; AOneTimeKeyboard: Boolean = False); overload;
    constructor Create(AKeyboard: TArray<TArray<TtgKeyboardButton>>; AResizeKeyboard: Boolean = False; AOneTimeKeyboard: Boolean = False); overload;
    procedure AddRow(AKeyboardRow: TArray<TtgKeyboardButton>);
    destructor Destroy; override;
  published
      /// <summary>
    /// Array of button rows, each represented by an Array of KeyboardButton objects
    /// </summary>
    [Alias('keyboard')]
    property Keyboard: TArray<TArray<TtgKeyboardButton>> read GetKeyboard write SetKeyboard;
    [DISABLE]
    property RefCount;
  end;

  /// <summary>
  /// Upon receiving a message with this object, Telegram clients will remove the current custom keyboard and display the default letter-keyboard.
  /// By default, custom keyboards are displayed until a new keyboard is sent by a bot.
  /// An exception is made for one-time keyboards that are hidden immediately after the user presses a button
  /// </summary>
  TtgReplyKeyboardRemove = class(TtgReplyMarkup)
  public
    /// <summary>
    /// Requests clients to remove the custom keyboard (user will not be able to summon this keyboard; if you want to hide the keyboard from sight but keep it accessible, use one_time_keyboard in ReplyKeyboardMarkup)
    /// </summary>
    [Alias('remove_keyboard')]
    RemoveKeyboard: Boolean;
    constructor Create(ARemoveKeyboard: Boolean = True);
  end;

  /// <summary>
  /// Upon receiving a message with this object, Telegram clients will hide
  /// the current custom keyboard and display the default letter-keyboard. By
  /// default, custom keyboards are displayed until a new keyboard is sent by
  /// a bot. An exception is made for one-time keyboards that are hidden
  /// immediately after the user presses a button (see ReplyKeyboardMarkup).
  /// </summary>
  [Alias('ReplyKeyboardHide')]
  TtgReplyKeyboardHide = class(TtgReplyMarkup)
  public
    /// <summary>
    /// Requested profile pictures (in up to 4 sizes each)
    /// </summary>
    [Alias('hide_keyboard')]
    Hide_keyboard: Boolean;
  end;

implementation

uses
  System.SysUtils;
{TtgButtonedMarkup<T: class>}

procedure TtgButtonedMarkup<T>.AddRow(AKeyboardRow: TArray<T>);
var
  LListBtn: TObjectList<T>;
begin
  LListBtn := TObjectList<T>.Create;
  LListBtn.AddRange(AKeyboardRow);
  FKeyboard.Add(LListBtn);
end;

constructor TtgButtonedMarkup<T>.Create;
begin
  FKeyboard := TObjectList<TObjectList<T>>.Create;
end;

constructor TtgButtonedMarkup<T>.Create(AKeyboardRow: TArray<T>);
begin
  Self.Create;
  AddRow(AKeyboardRow);
end;

constructor TtgButtonedMarkup<T>.Create(AKeyboard: TArray<TArray<T>>);
var
  i: Integer;
begin
  Self.Create;
  for i := Low(AKeyboard) to High(AKeyboard) do
  begin
    AddRow(AKeyboard[i]);
  end;
end;

destructor TtgButtonedMarkup<T>.Destroy;
begin
  FreeAndNil(FKeyboard);
end;

function TtgButtonedMarkup<T>.GetKeyboard: TArray<TArray<T>>;
var
  i: Integer;
begin
  SetLength(Result, FKeyboard.Count);
  for i := 0 to FKeyboard.Count - 1 do
  begin
    Result[i] := FKeyboard[i].ToArray;
  end;
end;

procedure TtgButtonedMarkup<T>.SetKeyboard(Value: TArray<TArray<T>>);
var
  i: Integer;
begin
  FKeyboard.Clear;
  for i := Low(Value) to High(Value) do
    Self.AddRow(Value[i]);
end;
{ TtgInlineKeyboardMarkup }

procedure TtgInlineKeyboardMarkup.AddRow(AInlineKeyboardRow: TArray<TtgInlineKeyboardButton>);
var
  LListBtn: TObjectList<TtgInlineKeyboardButton>;
begin
  LListBtn := TObjectList<TtgInlineKeyboardButton>.Create;
  LListBtn.AddRange(AInlineKeyboardRow);
  FInlineKeyboard.Add(LListBtn);
end;

constructor TtgInlineKeyboardMarkup.Create;
begin
  FInlineKeyboard := TObjectList<TObjectList<TtgInlineKeyboardButton>>.Create;
end;

constructor TtgInlineKeyboardMarkup.Create(AInlineKeyboardRow: TArray<TtgInlineKeyboardButton>);
begin
  Self.Create;
  AddRow(AInlineKeyboardRow);
end;

constructor TtgInlineKeyboardMarkup.Create(AInlineKeyboard: TArray<TArray<TtgInlineKeyboardButton>>);
var
  i: Integer;
begin
  Self.Create;
  for i := Low(AInlineKeyboard) to High(AInlineKeyboard) do
  begin
    AddRow(AInlineKeyboard[i]);
  end;
end;

destructor TtgInlineKeyboardMarkup.Destroy;
begin
  FreeAndNil(FInlineKeyboard);
  inherited;
end;

function TtgInlineKeyboardMarkup.GetKeyboard: TArray<TArray<TtgInlineKeyboardButton>>;
begin

end;

procedure TtgInlineKeyboardMarkup.SetKeyboard(Value: TArray<TArray<TtgInlineKeyboardButton>>);
begin

end;

{ TtgReplyKeyboardMarkup }
procedure TtgReplyKeyboardMarkup.AddRow(AKeyboardRow: TArray<TtgKeyboardButton>);
var
  LListBtn: TObjectList<TtgKeyboardButton>;
begin
  LListBtn := TObjectList<TtgKeyboardButton>.Create;
  LListBtn.AddRange(AKeyboardRow);
  Self.FKeyboard.Add(LListBtn);
end;

constructor TtgReplyKeyboardMarkup.Create;
begin
  inherited Create();
  FKeyboard := TObjectList<TObjectList<TtgKeyboardButton>>.Create;
end;

constructor TtgReplyKeyboardMarkup.Create(AKeyboardRow: TArray<TtgKeyboardButton>; AResizeKeyboard, AOneTimeKeyboard: Boolean);
begin
  Self.Create;
  AddRow(AKeyboardRow);
  ResizeKeyboard := AResizeKeyboard;
  OneTimeKeyboard := AOneTimeKeyboard;
end;

constructor TtgReplyKeyboardMarkup.Create(AKeyboard: TArray<TArray<TtgKeyboardButton>>; AResizeKeyboard, AOneTimeKeyboard: Boolean);
begin
  Self.Create;
  Self.Keyboard := AKeyboard;
  ResizeKeyboard := AResizeKeyboard;
  OneTimeKeyboard := AOneTimeKeyboard;
end;

destructor TtgReplyKeyboardMarkup.Destroy;
begin
  FKeyboard.Free;
  inherited Destroy;
end;

function TtgReplyKeyboardMarkup.GetKeyboard: TArray<TArray<TtgKeyboardButton>>;
var
  i: Integer;
begin
  SetLength(Result, FKeyboard.Count);
  for i := 0 to FKeyboard.Count - 1 do
  begin
    Result[i] := FKeyboard[i].ToArray;
  end;
end;

procedure TtgReplyKeyboardMarkup.SetKeyboard(Value: TArray<TArray<TtgKeyboardButton>>);
var
  i: Integer;
begin
  FKeyboard.Clear;
  for i := Low(Value) to High(Value) do
    Self.AddRow(Value[i]);
end;

{ TtgReplyKeyboardRemove }

constructor TtgReplyKeyboardRemove.Create(ARemoveKeyboard: Boolean);
begin
  inherited Create;
  RemoveKeyboard := ARemoveKeyboard;
end;

end.


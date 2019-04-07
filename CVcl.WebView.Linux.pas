{
  This file is part of CrossVCL WebBroser
  Copyright (c) 2018 Eugene Kryukov

  The contents of this file are subject to the Mozilla Public License Version 2.0 (the "License");
  you may not use this file except in compliance with the License. You may obtain a copy of the
  License at http://www.mozilla.org/MPL/

  Software distributed under the License is distributed on an "AS IS" basis, WITHOUT WARRANTY OF ANY
  KIND, either express or implied. See the License for the specific language governing rights and
  limitations under the License.
}

unit CVcl.WebView.Linux;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes, CVcl.WebView;

type

  PGtkWidget = Pointer;
  PWebKitWebView = Pointer;

  TLinuxWebView = class(TInterfacedObject, IWebView)
  private
    FScroll: PGtkWidget;
    FWebView: PWebKitWebView;
    FParentWnd: HWnd;
    FDelegate: IWebViewDelegate;
    FJSRunning: Boolean;
    FJSResult: Boolean;
    FJSResultStr: string;
    FJSErrorStr: string;
    { IWebView }
    procedure WebViewUpdateBounds;
    procedure WebViewNavigate(const URL: string);
    procedure WebViewLoadFromStream(const AStream: TStream; const MimeType, BaseUrl: string);
    procedure WebViewLoadFromString(const AString: string; const BaseUrl: string);
    function WebViewRunScript(const AScript: string; var ResultStr, ErrorStr: string): Boolean;
    function WebViewGetSelectedText: string;
    procedure WebViewGoForward;
    procedure WebViewGoBack;
    function WebGetHandle: Pointer;
  protected
  public
    constructor Create(AParentWnd: HWnd; ADelegate: IWebViewDelegate);
    destructor Destroy; override;
  end;

implementation

var
  Lib: NativeInt;
  webkit_web_view_new: function : PWebKitWebView; cdecl;
  webkit_web_view_load_uri: procedure (web_view: PWebKitWebView; uri: PUtf8Char); cdecl;
  webkit_web_view_load_html: procedure (web_view: PWebKitWebView; contents: PUtf8Char; base_uri: PUtf8Char); cdecl;
  webkit_web_view_load_bytes: procedure (web_view: PWebKitWebView; bytes: Pointer; mime_type: PUtf8Char; encoding: PUtf8Char; base_uri: PUtf8Char); cdecl;
  webkit_web_view_load_plain_text: procedure (web_view: PWebKitWebView; content: PUtf8Char); cdecl;
  webkit_web_view_can_go_back: function (web_view: PWebKitWebView): boolean; cdecl;
  webkit_web_view_can_go_forward: function (web_view: PWebKitWebView): boolean; cdecl;
  webkit_web_view_go_back: procedure (web_view: PWebKitWebView); cdecl;
  webkit_web_view_go_forward: procedure (web_view: PWebKitWebView); cdecl;
  webkit_web_view_reload: procedure (web_view: PWebKitWebView); cdecl;
  webkit_web_view_stop_loading: procedure (web_view: PWebKitWebView); cdecl;
  webkit_web_view_run_javascript: function (web_view: PWebKitWebView; script: PUtf8Char; cancellable, callback, user_data: Pointer): Pointer; cdecl;
  webkit_web_view_run_javascript_finish: function (web_view: PWebKitWebView; result, error: Pointer): Pointer; cdecl;
  webkit_javascript_result_get_global_context: function (value: Pointer): Pointer; cdecl;
  webkit_javascript_result_get_value: function (value: Pointer): Pointer; cdecl;
  webkit_javascript_result_unref: procedure (value: Pointer); cdecl;
  JSValueIsString: function (ctx: Pointer; value: Pointer): LongBool; cdecl;
  JSValueToStringCopy: function (ctx: Pointer; value: Pointer; exception: Pointer): Pointer; cdecl;
  JSStringGetLength: function (str: Pointer): LongWord; cdecl;
  JSStringGetCharactersPtr: function (&string: Pointer): PChar; cdecl;
  JSStringRelease: procedure (&string: Pointer); cdecl;

function LoadWebKit: Boolean;
begin
  Lib := LoadLibrary('libwebkit2gtk-4.0.so.37');
  Result := Lib <> 0;
  if Result then
  begin
    webkit_web_view_new := GetProcAddress(Lib, 'webkit_web_view_new');
    webkit_web_view_load_uri := GetProcAddress(Lib, 'webkit_web_view_load_uri');
    webkit_web_view_load_bytes := GetProcAddress(Lib, 'webkit_web_view_load_bytes');
    webkit_web_view_load_html := GetProcAddress(Lib, 'webkit_web_view_load_html');
    webkit_web_view_load_plain_text := GetProcAddress(Lib, 'webkit_web_view_load_plain_text');
    webkit_web_view_can_go_back := GetProcAddress(Lib, 'webkit_web_view_can_go_back');
    webkit_web_view_can_go_forward := GetProcAddress(Lib, 'webkit_web_view_can_go_forward');
    webkit_web_view_go_back := GetProcAddress(Lib, 'webkit_web_view_go_back');
    webkit_web_view_go_forward := GetProcAddress(Lib, 'webkit_web_view_go_forward');
    webkit_web_view_reload := GetProcAddress(Lib, 'webkit_web_view_reload');
    webkit_web_view_stop_loading := GetProcAddress(Lib, 'webkit_web_view_stop_loading');
    webkit_web_view_run_javascript := GetProcAddress(Lib, 'webkit_web_view_run_javascript');
    webkit_web_view_run_javascript_finish := GetProcAddress(Lib, 'webkit_web_view_run_javascript_finish');
    webkit_javascript_result_get_global_context := GetProcAddress(Lib, 'webkit_javascript_result_get_global_context');
    webkit_javascript_result_get_value := GetProcAddress(Lib, 'webkit_javascript_result_get_value');
    webkit_javascript_result_unref := GetProcAddress(Lib, 'webkit_javascript_result_unref');

    Lib := LoadLibrary('libjavascriptcoregtk-4.0.so.18');
    Result := Lib <> 0;
    if Result then
    begin
      JSValueIsString := GetProcAddress(Lib, 'JSValueIsString');
      JSValueToStringCopy := GetProcAddress(Lib, 'JSValueToStringCopy');
      JSStringGetLength := GetProcAddress(Lib, 'JSStringGetLength');
      JSStringGetCharactersPtr := GetProcAddress(Lib, 'JSStringGetCharactersPtr');
      JSStringRelease := GetProcAddress(Lib, 'JSStringRelease');
    end;
  end;
end;

const
  GtkLib = 'libgtk-3.so.0';
  GLib2 = 'libglib-2.0.so.0';
  GObjectLib = 'libgobject-2.0.so.0';

type
  PGError = ^TGError;
  TGError = packed record
    domain: Cardinal;
    code: Integer;
    msg: PAnsiChar;
  end;


function g_bytes_new(data: Pointer; size: NativeUInt): Pointer; cdecl; external GLib2;
procedure g_bytes_unref(bytes: Pointer); cdecl; external GLib2;
function g_bytes_get_size(bytes: Pointer): NativeUInt; cdecl; external GLib2;

function g_signal_connect_data(instance: Pointer; detailed_signal: PUtf8Char; c_handler: Pointer; data: Pointer;
  destroy_data: Pointer; connect_flags: Integer): NativeUInt; cdecl; external GObjectLib;

procedure gtk_widget_destroy(widget: PGtkWidget); cdecl; external GtkLib;
procedure gtk_widget_show(widget: PGtkWidget); cdecl; external GtkLib;
procedure gtk_widget_set_size_request(widget: PGtkWidget; width: Integer; height: Integer); cdecl; external GtkLib;
procedure gtk_container_add(container: Pointer; widget: PGtkWidget); cdecl; external GtkLib;
function gtk_scrolled_window_new(hadjustment, vadjustment: Pointer): Pointer; cdecl; external GtkLib;

{ Callbacks }

procedure LoadStarted(web_view: PWebKitWebView; load_event: Pointer; user_data: Pointer); cdecl;
begin
end;

procedure LoadFinished(web_view: PWebKitWebView; load_event: Pointer; user_data: Pointer); cdecl;
begin
  TLinuxWebView(user_data).FDelegate.FinishLoading;
end;

function LoadError(web_view: PWebKitWebView; web_frame: Pointer; uri: PAnsiChar; error: Pointer;
  user_data: Pointer): boolean; cdecl;
begin
  Result := True;
end;

{ TLinuxWebView }

constructor TLinuxWebView.Create(AParentWnd: HWnd; ADelegate: IWebViewDelegate);
var
  Window, Fixed: Pointer;
begin
  inherited Create;
  FDelegate := ADelegate;
  FParentWnd := AParentWnd;

  if not LoadWebKit then
    raise EWebView.Create('WebView requires WebKitGtk. Install it first - "sudo apt install libwebkit2gtk-4.0" or "sudo yum install webkitgtk2.x86_64"');

  FWebView := webkit_web_view_new;
  gtk_widget_show(FWebView);

  g_signal_connect_data(FWebView, 'load-started', @LoadStarted, Self, nil, 0);
  g_signal_connect_data(FWebView, 'load-finished', @LoadFinished, Self, nil, 0);
  g_signal_connect_data(FWebView, 'load-error', @LoadError, Self, nil, 0);

  if GetNativeHandles(FParentWnd, Window, Fixed) then
  begin
    // Create GtkScrollerWindow
    FScroll := gtk_scrolled_window_new(nil, nil);
    gtk_widget_show(FScroll);

    // Add to Scroll
    gtk_container_add(FScroll, FWebView);

    // Add Scroll to Parent
    SetNativeParent(FScroll, FParentWnd);

    WebViewUpdateBounds;
  end;
end;

destructor TLinuxWebView.Destroy;
begin
  gtk_widget_destroy(FWebView);
  FWebView := nil;
  inherited;
end;

function TLinuxWebView.WebGetHandle: Pointer;
begin
  Result := FWebView;
end;

function TLinuxWebView.WebViewGetSelectedText: string;
var
  Error: string;
begin
  if not WebViewRunScript('window.getSelection().toString()', Result, Error) then
    raise EWebView.Create('Can''t get selection');
end;

procedure TLinuxWebView.WebViewGoBack;
begin
  webkit_web_view_go_back(FWebView);
end;

procedure TLinuxWebView.WebViewGoForward;
begin
  webkit_web_view_go_forward(FWebView);
end;

procedure TLinuxWebView.WebViewLoadFromStream(const AStream: TStream; const MimeType, BaseUrl: string);
var
  GB: Pointer;
  Memory: TMemoryStream;
begin
  Memory := TMemoryStream.Create;
  try
    Memory.CopyFrom(AStream, AStream.Size);
    GB := g_bytes_new(Memory.Memory, Memory.Size);
    try
      webkit_web_view_load_bytes(FWebView, GB, PUtf8Char(Utf8Encode(MimeType)), nil, PUtf8Char(Utf8Encode(BaseUrl)));
    finally
      g_bytes_unref(GB);
    end;
  finally
    Memory.Free;
  end;
end;

procedure TLinuxWebView.WebViewLoadFromString(const AString: string; const BaseUrl: string);
begin
  webkit_web_view_load_html(FWebView, PUtf8Char(Utf8Encode(AString)), PUtf8Char(Utf8Encode(BaseUrl)));
end;

procedure TLinuxWebView.WebViewNavigate(const URL: string);
var
  AUrl: Utf8String;
begin
  AUrl := Url;
  webkit_web_view_load_uri(FWebView, PUtf8Char(AUrl));
end;

function JSStringToStr(const Str: Pointer): string;
var
  Len: Integer;
begin
  Len := JSStringGetLength(Str);
  SetLength(Result, Len);
  Move(JSStringGetCharactersPtr(Str)^, PChar(Result)^, Len * 2);
end;

function JSFinishCallback(web_view: PWebKitWebView; res: Pointer; user_data: TLinuxWebView): Pointer; cdecl;
var
  JSResult: Pointer;
  Error: PGError;
  JSStr, Value, Ctx: Pointer;
begin
  user_data.FJSRunning := False;

  Error := nil;
  JSResult := webkit_web_view_run_javascript_finish(user_data.FWebView, res, @Error);

  if JSResult = nil then
  begin
    user_data.FJSErrorStr := Error.msg;
    Exit;
  end;

  Ctx := webkit_javascript_result_get_global_context(JSResult);
  Value := webkit_javascript_result_get_value(JSResult);

  if JSValueIsString(Ctx, Value) then
  begin
    JSStr := JSValueToStringCopy(Ctx, Value, nil);
    user_data.FJSResultStr := JSStringToStr(JSStr);
    user_data.FJSResult := True;
    JSStringRelease(JSStr);
  end;

  webkit_javascript_result_unref(JSResult);
end;

function TLinuxWebView.WebViewRunScript(const AScript: string; var ResultStr, ErrorStr: string): Boolean;
begin
  Result := False;

  FJSRunning := True;
  FJSResult := False;
  FJSResultStr := '';
  FJSErrorStr := '';

  webkit_web_view_run_javascript(FWebView, PUtf8Char(Utf8Encode(AScript)), nil, @JSFinishCallback, Self);
  while FJSRunning do
    WaitMessage;

  ResultStr := FJSResultStr;
  ErrorStr := FJSErrorStr;
  Result := FJSResult;
end;

procedure TLinuxWebView.WebViewUpdateBounds;
var
  R: TRect;
begin
  GetClientRect(FParentWnd, R);
  gtk_widget_set_size_request(FScroll, R.Width, R.Height);
end;

end.

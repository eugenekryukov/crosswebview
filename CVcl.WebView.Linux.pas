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
    { IWebView }
    procedure WebViewUpdateBounds;
    procedure WebViewNavigate(const URL: string);
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
  webkit_web_view_load_string: procedure (web_view: PWebKitWebView; content: PUtf8Char; mime_type: PUtf8Char; encoding: PUtf8Char; base_uri: PUtf8Char); cdecl;
  webkit_web_view_can_go_back: function (web_view: PWebKitWebView): boolean; cdecl;
  webkit_web_view_can_go_forward: function (web_view: PWebKitWebView): boolean; cdecl;
  webkit_web_view_go_back: procedure (web_view: PWebKitWebView); cdecl;
  webkit_web_view_go_forward: procedure (web_view: PWebKitWebView); cdecl;
  webkit_web_view_reload: procedure (web_view: PWebKitWebView); cdecl;
  webkit_web_view_stop_loading: procedure (web_view: PWebKitWebView); cdecl;
  webkit_web_view_execute_script: procedure (web_view: PWebKitWebView; script: PUtf8Char); cdecl;

function LoadWebKit: Boolean;
begin
  Lib := LoadLibrary('libwebkitgtk-3.0.so.0');
  Result := Lib <> 0;
  if Result then
  begin
    webkit_web_view_new := GetProcAddress(Lib, 'webkit_web_view_new');
    webkit_web_view_load_uri := GetProcAddress(Lib, 'webkit_web_view_load_uri');
    webkit_web_view_load_string := GetProcAddress(Lib, 'webkit_web_view_load_string');
    webkit_web_view_can_go_back := GetProcAddress(Lib, 'webkit_web_view_can_go_back');
    webkit_web_view_can_go_forward := GetProcAddress(Lib, 'webkit_web_view_can_go_forward');
    webkit_web_view_go_back := GetProcAddress(Lib, 'webkit_web_view_go_back');
    webkit_web_view_go_forward := GetProcAddress(Lib, 'webkit_web_view_go_forward');
    webkit_web_view_reload := GetProcAddress(Lib, 'webkit_web_view_reload');
    webkit_web_view_stop_loading := GetProcAddress(Lib, 'webkit_web_view_stop_loading');
    webkit_web_view_execute_script := GetProcAddress(Lib, 'webkit_web_view_execute_script');
  end;
end;

const
  GtkLib = 'libgtk-3.so.0';
  GObjectLib = 'libgobject-2.0.so.0';

function g_signal_connect_data(instance: Pointer; detailed_signal: PUtf8Char; c_handler: Pointer; data: Pointer;
  destroy_data: Pointer; connect_flags: Integer): NativeUInt; cdecl; external GObjectLib;

procedure gtk_widget_destroy(widget: PGtkWidget); cdecl; external GtkLib;
procedure gtk_widget_show(widget: PGtkWidget); cdecl; external GtkLib;
procedure gtk_widget_set_size_request(widget: PGtkWidget; width: Integer; height: Integer); cdecl; external GtkLib;
procedure gtk_container_add(container: Pointer; widget: PGtkWidget); cdecl; external GtkLib;
function gtk_scrolled_window_new(hadjustment, vadjustment: Pointer): Pointer; cdecl; external GtkLib;
procedure gtk_fixed_put(fixed: Pointer; widget: Pointer; x: Integer; y: Integer); cdecl; external GtkLib;

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
    raise EWebView.Create('Can''t initialize GtkWebKit');

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
    gtk_fixed_put(Fixed, FScroll, 0, 0);

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

procedure TLinuxWebView.WebViewGoBack;
begin
  webkit_web_view_go_back(FWebView);
end;

procedure TLinuxWebView.WebViewGoForward;
begin
  webkit_web_view_go_forward(FWebView);
end;

procedure TLinuxWebView.WebViewNavigate(const URL: string);
var
  AUrl: Utf8String;
begin
  AUrl := Url;
  webkit_web_view_load_uri(FWebView, PUtf8Char(AUrl));
end;

procedure TLinuxWebView.WebViewUpdateBounds;
var
  R: TRect;
begin
  GetClientRect(FParentWnd, R);
  gtk_widget_set_size_request(FScroll, R.Width, R.Height);
end;

end.

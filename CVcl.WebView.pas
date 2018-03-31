{
  This file is part of VCL WebBroser
  Copyright (c) 2018 Eugene Kryukov

  The contents of this file are subject to the Mozilla Public License Version 2.0 (the "License");
  you may not use this file except in compliance with the License. You may obtain a copy of the
  License at http://www.mozilla.org/MPL/

  Software distributed under the License is distributed on an "AS IS" basis, WITHOUT WARRANTY OF ANY
  KIND, either express or implied. See the License for the specific language governing rights and
  limitations under the License.
}

unit CVcl.WebView;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes, Vcl.Controls;

type

  EWebView = class(Exception);

  IWebViewDelegate = interface
    ['{60ACB0A0-DCAB-465C-82AF-7D1D935C71BE}']
    procedure FinishLoading;
  end;

  IWebView = interface
    ['{09342BAC-1400-4FD3-BF26-E6D9EABEB128}']
    procedure WebViewUpdateBounds;
    procedure WebViewNavigate(const URL: string);
    procedure WebViewGoForward;
    procedure WebViewGoBack;
    function WebGetHandle: Pointer;
  end;

  TCustomWebView = class(TWinControl, IWebViewDelegate)
  private
    FWebView: IWebView;
    FOnFinishLoading: TNotifyEvent;
    function GetNativeHandle: Pointer;
    procedure WMEraseBkgnd(var Message: TWmEraseBkgnd); message WM_ERASEBKGND;
    { IWebViewDelegate }
    procedure FinishLoading; virtual;
  protected
    procedure CreateWnd; override;
    procedure DestroyWnd; override;
    procedure Resize; override;
  protected
    function CreateWebView: IWebView; virtual;
    procedure DoFinishLoading; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Navigate(const URL: string);
    procedure GoForward;
    procedure GoBack;
    // Handle to native WebView object
    //   TWebBrowser control on Windows
    //   WKWebView view on macOS
    //   GtkWebView widget on Linux
    property NativeHandle: Pointer read GetNativeHandle;
    property OnFinishLoading: TNotifyEvent read FOnFinishLoading write FOnFinishLoading;
  end;

  TWebView = class(TCustomWebView)
  published
    property Align;
    property Anchors;
    property BiDiMode;
    property Color;
    property Constraints;
    property Ctl3D;
    property UseDockManager default True;
    property DockSite;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property ParentBiDiMode;
    property ParentBackground;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property Visible;
    property OnCanResize;
    property OnClick;
    property OnConstrainedResize;
    property OnContextPopup;
    property OnDockDrop;
    property OnDockOver;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnFinishLoading;
    property OnGetSiteInfo;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    property OnStartDock;
    property OnStartDrag;
    property OnUnDock;
  end;

procedure Register;

implementation

{$IFDEF MSWINDOWS}
uses CVcl.WebView.Win;
{$ENDIF}
{$IFDEF MACOS}
uses CVcl.WebView.Mac;
{$ENDIF}
{$IFDEF LINUX}
uses CVcl.WebView.Linux;
{$ENDIF}

procedure Register;
begin
  RegisterComponents('CrossVCL', [TWebView]);
end;

{ TWebView }

constructor TCustomWebView.Create(AOwner: TComponent);
begin
  inherited;
end;

destructor TCustomWebView.Destroy;
begin
  inherited;
end;

function TCustomWebView.CreateWebView: IWebView;
begin
  {$IFDEF MSWINDOWS}
  Result := TWinWebView.Create(Handle, Self);
  {$ENDIF}
  {$IFDEF MACOS}
  Result := TMacWebView.Create(Handle, Self);
  {$ENDIF}
  {$IFDEF LINUX}
  Result := TLinuxWebView.Create(Handle, Self);
  {$ENDIF}
  if Result = nil then
    raise EWebView.Create('No implementation of IWebView found');
end;

procedure TCustomWebView.CreateWnd;
begin
  inherited;
  FWebView := CreateWebView;
end;

procedure TCustomWebView.DestroyWnd;
begin
  FWebView := nil;
  inherited;
end;

procedure TCustomWebView.DoFinishLoading;
begin
  if Assigned(FOnFinishLoading) then
    FOnFinishLoading(Self);
end;

procedure TCustomWebView.FinishLoading;
begin
  DoFinishLoading;
end;

function TCustomWebView.GetNativeHandle: Pointer;
begin
  Result := FWebView.WebGetHandle;
end;

procedure TCustomWebView.GoBack;
begin
  FWebView.WebViewGoBack;
end;

procedure TCustomWebView.GoForward;
begin
  FWebView.WebViewGoForward;
end;

procedure TCustomWebView.Navigate(const URL: string);
begin
  FWebView.WebViewNavigate(URL);
end;

procedure TCustomWebView.Resize;
begin
  inherited;
  if FWebView <> nil then
    FWebView.WebViewUpdateBounds;
end;

procedure TCustomWebView.WMEraseBkgnd(var Message: TWmEraseBkgnd);
begin
  Message.Result := 1;
end;

end.

$PBExportHeader$w_main.srw
forward
global type w_main from w_principal
end type
end forward

global type w_main from w_principal
integer width = 5536
end type
global w_main w_main

event open;call super::open;Integer	li_cantidad
Menu		m_menu

m_menu	= m_principal
//Menu		m_menu
//
//m_menu	= m_principal
//
//IF Pos(String(Upper(gstr_us.nombre)),"JSALGADO")=0 AND &
//	Pos(String(Upper(gstr_us.nombre)),"FSAT")=0 THEN
//	m_menu.Item[3].Enabled = False
//	m_menu.Item[4].Item[1].Enabled = False
//	m_menu.Item[4].Item[2].Enabled = False
//	m_menu.Item[4].Item[3].Enabled = False
//END IF
end event

on w_main.create
call super::create
end on

on w_main.destroy
call super::destroy
if IsValid(MenuID) then destroy(MenuID)
end on


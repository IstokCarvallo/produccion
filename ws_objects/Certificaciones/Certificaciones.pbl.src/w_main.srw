$PBExportHeader$w_main.srw
forward
global type w_main from w_principal
end type
end forward

global type w_main from w_principal
integer width = 5536
end type
global w_main w_main

event open;call super::open;//Integer	li_cantidad
//Menu		m_menu
//
//m_menu	= m_principal
//
//IF Upper(gstr_us.nombre) <> "JSALGADO" THEN
//	SELECT	Count(*) INTO :li_cantidad
//		FROM	"SYS"."SYSGROUPS"
//		WHERE	group_name='Desarrollo'
//		AND	member_name=:gstr_us.nombre ;
//
//	IF sqlca.SQLCode = -1 THEN
//		F_ErrorBaseDatos(sqlca,"Lectura de la Tabla sysgroups")
//	ELSEIF sqlca.SQLCode = 100 THEN
//		m_menu.Item[6].Item[2].Item[1].Enabled = False
//		m_menu.Item[6].Item[2].Item[2].Enabled = False
//		m_menu.Item[6].Item[4].Item[1].Enabled = False
//		m_menu.Item[6].Item[4].Item[2].Enabled = False
//		m_menu.Item[6].Item[4].Item[3].Enabled = False
//		m_menu.Item[6].Item[4].Item[4].Enabled = False
//		m_menu.Item[6].Item[4].Item[5].Enabled = False
//		m_menu.Item[6].Item[4].Item[6].Enabled = False
//		m_menu.Item[6].Item[4].Item[7].Enabled = False
//	END IF
//END IF
end event

on w_main.create
call super::create
end on

on w_main.destroy
call super::destroy
if IsValid(MenuID) then destroy(MenuID)
end on


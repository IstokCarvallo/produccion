$PBExportHeader$w_main.srw
forward
global type w_main from w_principal
end type
end forward

global type w_main from w_principal
windowstate windowstate = maximized!
end type
global w_main w_main

event open;call super::open;Integer	li_cantidad
Menu		m_menu

m_menu	= m_principal
//
//SELECT	Count(*) INTO :li_cantidad
//	FROM	dba.USUARBODEGA
//	WHERE	Upper(usbo_usuari) = :gstr_us.nombre
//	AND   bode_codigo = :gstr_param.bodega;
//
//IF sqlca.SQLCode = -1 THEN
//	F_ErrorBaseDatos(sqlca,"Lectura de la Tabla usuarbodega")
//ELSEIF li_cantidad = 0 THEN
//	m_menu.Item[2].Item[22].Enabled	=	False
//END IF
end event

on w_main.create
call super::create
end on

on w_main.destroy
call super::destroy
if IsValid(MenuID) then destroy(MenuID)
end on


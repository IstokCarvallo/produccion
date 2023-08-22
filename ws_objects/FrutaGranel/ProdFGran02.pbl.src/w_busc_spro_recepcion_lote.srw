$PBExportHeader$w_busc_spro_recepcion_lote.srw
$PBExportComments$Busqueda de Movimientos de Recepción de Huerto con Filtro por Especie.
forward
global type w_busc_spro_recepcion_lote from w_busc_spro_movtofrutagranenca_recepcion
end type
end forward

global type w_busc_spro_recepcion_lote from w_busc_spro_movtofrutagranenca_recepcion
integer width = 2743
integer height = 1880
end type
global w_busc_spro_recepcion_lote w_busc_spro_recepcion_lote

type variables
//datawindowchild idwc_planta
 
protected:
String ias_campo[]
end variables

on w_busc_spro_recepcion_lote.create
call super::create
end on

on w_busc_spro_recepcion_lote.destroy
call super::destroy
end on

event open;istr_busq	=	Message.PowerObjectParm

This.Icon	=	Gstr_apl.Icono

Tab_1.TabPage_1.dw_5.GetChild("plde_codigo", idwc_planta)
idwc_planta.SetTransObject(sqlca)
IF idwc_planta.Retrieve() = 0 THEN
	MessageBox("Atención","Falta Registrar Plantas")
	idwc_planta.InsertRow(0)
END IF

PostEvent("ue_ordenamiento")
is_ordena 	=	'Número:mfge_numero,Fecha Movto.:mfge_fecmov,Especie:espe_codigo'

Tab_1.TabPage_1.dw_5.SetTransObject(SqlCa)
Tab_1.TabPage_1.dw_5.InsertRow(0)
Tab_1.TabPage_1.dw_5.SetItem(1, "plde_codigo", Integer(istr_busq.Argum[1]))
Tab_1.TabPage_1.dw_5.Enabled	=	False
Tab_1.TabPage_1.dw_5.modify("plde_codigo.BackGround.color = " +  String(553648127))

Tab_1.TabPage_1.dw_4.SetTransObject(SqlCa)
Tab_1.TabPage_1.dw_4.InsertRow(0)
Tab_1.TabPage_1.dw_4.SetItem(1, "tpmv_codigo", Integer(istr_busq.Argum[2]))
Tab_1.TabPage_1.dw_4.Enabled	=	False
Tab_1.TabPage_1.dw_4.modify("tpmv_codigo.BackGround.color = " +  String(553648127))

Tab_1.TabPage_1.ddlb_estado.SelectItem(1)
Tab_1.TabPage_1.em_fechamovto.text = String(Date(F_FechaHora()))

dw_1.GetChild("espe_codigo", idwc_especiedet)
idwc_especiedet.SetTransObject(sqlca)
IF idwc_especiedet.Retrieve(Integer(istr_busq.Argum[4])) = 0 THEN
	MessageBox("Atención","Falta Registrar Especies")
	idwc_especiedet.InsertRow(0)
ELSE
	idwc_especiedet.SetSort("espe_nombre A")
	idwc_especiedet.Sort()
END IF

istr_busq.argum[10]	=	String(0)










end event

event ue_asignacion();
istr_busq.argum[1]	= String(dw_1.Object.plde_codigo[dw_1.GetRow()])
istr_busq.argum[2]	= String(dw_1.Object.tpmv_codigo[dw_1.GetRow()])
istr_busq.argum[3]	= String(dw_1.Object.mfge_numero[dw_1.GetRow()])
istr_busq.argum[4]	= String(dw_1.Object.mfge_fecmov[dw_1.GetRow()])
istr_busq.argum[5]	= String(dw_1.Object.espe_codigo[dw_1.GetRow()])
istr_busq.argum[7]	= String(dw_1.Object.tran_codigo[dw_1.GetRow()])
istr_busq.argum[8]	= String(dw_1.Object.mfge_guisii[dw_1.GetRow()])
istr_busq.argum[9]	= String(dw_1.Object.mfge_estmov[dw_1.GetRow()])
istr_busq.argum[10]	=	String(1)

CloseWithReturn(This,istr_busq)
end event

type pb_insertar from w_busc_spro_movtofrutagranenca_recepcion`pb_insertar within w_busc_spro_recepcion_lote
integer x = 2345
integer y = 908
end type

type dw_1 from w_busc_spro_movtofrutagranenca_recepcion`dw_1 within w_busc_spro_recepcion_lote
integer x = 32
integer y = 708
end type

event dw_1::doubleclicked;IF row > 0 THEN
	Parent.PostEvent("ue_asignacion")
END IF
end event

type pb_salir from w_busc_spro_movtofrutagranenca_recepcion`pb_salir within w_busc_spro_recepcion_lote
integer x = 2354
integer y = 1264
end type

event pb_salir::clicked;CloseWithReturn(Parent,istr_busq)
end event

type tab_1 from w_busc_spro_movtofrutagranenca_recepcion`tab_1 within w_busc_spro_recepcion_lote
integer x = 32
integer y = 28
integer height = 652
end type

on tab_1.create
call super::create
this.Control[]={this.tabpage_1,&
this.tabpage_2,&
this.tabpage_3}
end on

on tab_1.destroy
call super::destroy
end on

type tabpage_1 from w_busc_spro_movtofrutagranenca_recepcion`tabpage_1 within tab_1
integer height = 524
end type

type pb_filtrar from w_busc_spro_movtofrutagranenca_recepcion`pb_filtrar within tabpage_1
integer x = 1865
integer y = 288
end type

event pb_filtrar::clicked;Date	  ld_FechaMovto
Integer li_estado

IF NOT IsDate(em_fechamovto.Text) THEN
	MessageBox("Atención","Falta ingresar la Fecha de Movimiento de Inicio.")
	RETURN
END IF

ld_FechaMovto	=	Date(em_fechamovto.Text)

dw_1.SetTransObject(Sqlca)

IF tab_1.tabpage_1.ddlb_estado.text = "Transitoria" THEN
	li_estado	=	1
ELSE
	li_estado	=	3
END IF	

IF dw_1.Retrieve (dw_5.Object.plde_codigo[1], &
						dw_4.Object.tpmv_codigo[1], &
						li_estado, &
				      Integer(istr_busq.argum[5]), &
						ld_FechaMovto,Integer(istr_busq.argum[4])) > 0 THEN
	dw_1.SetFocus()
	dw_1.SelectRow(1,True)
ELSE
	MessageBox("Atención","No hay información para mostrar",Exclamation!,Ok!)
END IF
end event

type st_1 from w_busc_spro_movtofrutagranenca_recepcion`st_1 within tabpage_1
end type

type st_2 from w_busc_spro_movtofrutagranenca_recepcion`st_2 within tabpage_1
end type

type dw_4 from w_busc_spro_movtofrutagranenca_recepcion`dw_4 within tabpage_1
end type

type dw_5 from w_busc_spro_movtofrutagranenca_recepcion`dw_5 within tabpage_1
integer x = 425
integer y = 36
end type

type em_fechamovto from w_busc_spro_movtofrutagranenca_recepcion`em_fechamovto within tabpage_1
integer width = 411
string facename = "Tahoma"
end type

type st_4 from w_busc_spro_movtofrutagranenca_recepcion`st_4 within tabpage_1
end type

type st_5 from w_busc_spro_movtofrutagranenca_recepcion`st_5 within tabpage_1
end type

type dw_6 from w_busc_spro_movtofrutagranenca_recepcion`dw_6 within tabpage_1
boolean visible = false
end type

type st_6 from w_busc_spro_movtofrutagranenca_recepcion`st_6 within tabpage_1
boolean visible = false
end type

type cbx_todas from w_busc_spro_movtofrutagranenca_recepcion`cbx_todas within tabpage_1
boolean visible = false
end type

type ddlb_estado from w_busc_spro_movtofrutagranenca_recepcion`ddlb_estado within tabpage_1
string item[] = {"Transitoria","Definitiva"}
end type

event ddlb_estado::selectionchanged;//
end event

type tabpage_2 from w_busc_spro_movtofrutagranenca_recepcion`tabpage_2 within tab_1
integer height = 524
end type

type pb_acepta from w_busc_spro_movtofrutagranenca_recepcion`pb_acepta within tabpage_2
end type

type dw_3 from w_busc_spro_movtofrutagranenca_recepcion`dw_3 within tabpage_2
end type

type dw_2 from w_busc_spro_movtofrutagranenca_recepcion`dw_2 within tabpage_2
end type

type tabpage_3 from w_busc_spro_movtofrutagranenca_recepcion`tabpage_3 within tab_1
integer height = 524
end type

type sle_argumento2 from w_busc_spro_movtofrutagranenca_recepcion`sle_argumento2 within tabpage_3
end type

type st_argum2 from w_busc_spro_movtofrutagranenca_recepcion`st_argum2 within tabpage_3
end type

type sle_argumento1 from w_busc_spro_movtofrutagranenca_recepcion`sle_argumento1 within tabpage_3
end type

event sle_argumento1::getfocus;This.SelectText(1, Len(This.Text))

This.BackColor					= RGB(255,255,255)
This.TabOrder					= 10
sle_argumento2.Text			= ""
sle_argumento2.BackColor	= RGB(192,192,192)
sle_argumento2.TabOrder		= 0
sle_argumento3.Text			= ""
sle_argumento3.BackColor	= RGB(192,192,192)
sle_argumento3.TabOrder		= 0
es_numero						= True
is_busca							= "mfge_codigo"
end event

type st_argum1 from w_busc_spro_movtofrutagranenca_recepcion`st_argum1 within tabpage_3
end type

type pb_buscar from w_busc_spro_movtofrutagranenca_recepcion`pb_buscar within tabpage_3
end type

type st_3 from w_busc_spro_movtofrutagranenca_recepcion`st_3 within tabpage_3
end type

type sle_argumento3 from w_busc_spro_movtofrutagranenca_recepcion`sle_argumento3 within tabpage_3
end type


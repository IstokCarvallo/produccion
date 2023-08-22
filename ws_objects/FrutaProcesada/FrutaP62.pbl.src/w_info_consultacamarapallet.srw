$PBExportHeader$w_info_consultacamarapallet.srw
$PBExportComments$Proceso de Cierre Mensual.
forward
global type w_info_consultacamarapallet from w_para_informes
end type
type uo_selplantas from uo_seleccion_plantas within w_info_consultacamarapallet
end type
type uo_selcliente from uo_seleccion_clientesprod within w_info_consultacamarapallet
end type
type st_1 from statictext within w_info_consultacamarapallet
end type
type st_2 from statictext within w_info_consultacamarapallet
end type
type st_6 from statictext within w_info_consultacamarapallet
end type
type dw_camara from datawindow within w_info_consultacamarapallet
end type
type st_7 from statictext within w_info_consultacamarapallet
end type
end forward

global type w_info_consultacamarapallet from w_para_informes
integer width = 2464
integer height = 1540
string title = "Informe Consulta Camara Pallet"
boolean controlmenu = false
event ue_validapassword ( )
event ue_imprimir ( )
uo_selplantas uo_selplantas
uo_selcliente uo_selcliente
st_1 st_1
st_2 st_2
st_6 st_6
dw_camara dw_camara
st_7 st_7
end type
global w_info_consultacamarapallet w_info_consultacamarapallet

type variables
str_mant istr_mant
Str_busqueda	istr_busq

datawindowchild idwc_camara
end variables

event ue_imprimir();SetPointer(HourGlass!)

Long		fila,ll_camara
String ls_nombrecamara

istr_info.titulo	= "CONSULTA CAMARA PALLET"
istr_info.copias	= 1

OpenWithParm(vinf,istr_info)
vinf.dw_1.DataObject = "dw_info_consultacamarapallet"
vinf.dw_1.SetTransObject(sqlca)
ll_camara = dw_camara.object.cama_codigo[dw_camara.getrow()]

select cama_nombre into :ls_nombrecamara
from dbo.camarasbode
where plde_codigo=:uo_SelPlantas.Codigo
and cama_codigo = :ll_camara
using sqlca;

fila = vinf.dw_1.Retrieve( ll_camara,uo_SelPlantas.Codigo,uo_SelCliente.Codigo)

IF fila = -1 THEN
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
					"de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
ELSEIF fila = 0 THEN
	MessageBox( "No Existe información", "No existe información para este informe.", StopSign!, Ok!)
ELSE
	F_Membrete(vinf.dw_1)
	vinf.dw_1.Modify("cliente.text = 'Cliente "+uo_SelCliente.nombre+ "'")
	vinf.dw_1.Modify("planta.text = 'Planta "+uo_SelPlantas.nombre+ "'")
	vinf.dw_1.Modify("camara.text = 'Camara "+ls_nombrecamara+ "'")

	IF gs_Ambiente <> 'Windows' THEN F_ImprimeInformePdf(vinf.dw_1, istr_info.titulo)
END IF

SetPointer(Arrow!)
end event

on w_info_consultacamarapallet.create
int iCurrent
call super::create
this.uo_selplantas=create uo_selplantas
this.uo_selcliente=create uo_selcliente
this.st_1=create st_1
this.st_2=create st_2
this.st_6=create st_6
this.dw_camara=create dw_camara
this.st_7=create st_7
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.uo_selplantas
this.Control[iCurrent+2]=this.uo_selcliente
this.Control[iCurrent+3]=this.st_1
this.Control[iCurrent+4]=this.st_2
this.Control[iCurrent+5]=this.st_6
this.Control[iCurrent+6]=this.dw_camara
this.Control[iCurrent+7]=this.st_7
end on

on w_info_consultacamarapallet.destroy
call super::destroy
destroy(this.uo_selplantas)
destroy(this.uo_selcliente)
destroy(this.st_1)
destroy(this.st_2)
destroy(this.st_6)
destroy(this.dw_camara)
destroy(this.st_7)
end on

event open;call super::open;Boolean lb_Cerrar 

If IsNull(uo_SelCliente.Codigo) Then lb_Cerrar = True
If IsNull(uo_SelPlantas.Codigo) Then lb_Cerrar = True


If lb_Cerrar Then
	Close(This)
Else
	uo_SelCliente.Seleccion(True, False)
	uo_SelPlantas.Seleccion(False, False)
	uo_SelPlantas.Inicia(gi_CodPlanta)

	dw_camara.settransobject(sqlca)
	dw_camara.insertRow(0)
	
	dw_camara.GetChild("cama_codigo", idwc_camara)
	idwc_camara.SetTransObject(sqlca)
	idwc_camara.Retrieve(uo_SelPlantas.Codigo) 
End If
end event

type pb_excel from w_para_informes`pb_excel within w_info_consultacamarapallet
end type

type st_computador from w_para_informes`st_computador within w_info_consultacamarapallet
end type

type st_usuario from w_para_informes`st_usuario within w_info_consultacamarapallet
end type

type st_temporada from w_para_informes`st_temporada within w_info_consultacamarapallet
end type

type p_logo from w_para_informes`p_logo within w_info_consultacamarapallet
end type

type st_titulo from w_para_informes`st_titulo within w_info_consultacamarapallet
integer width = 1655
string text = "Informe Consulta Camara Pallet"
end type

type pb_acepta from w_para_informes`pb_acepta within w_info_consultacamarapallet
integer x = 2107
integer y = 732
string picturename = "\Desarrollo 17\Imagenes\Botones\Aceptar.png"
string disabledname = "\Desarrollo 17\Imagenes\Botones\Aceptar-bn.png"
long backcolor = 553648127
end type

event pb_acepta::clicked;SetPointer(HourGlass!)

IF IsNull(dw_camara.object.cama_codigo[1])  OR dw_camara.object.cama_codigo[1] = 0 THEN
 MessageBox("Atención", "Debe Ingresar Cámara Previamente ",Exclamation!)
	RETURN
END IF

Parent.TriggerEvent("ue_imprimir")
end event

type pb_salir from w_para_informes`pb_salir within w_info_consultacamarapallet
integer x = 2107
integer y = 1008
end type

type uo_selplantas from uo_seleccion_plantas within w_info_consultacamarapallet
integer x = 677
integer y = 816
integer width = 974
integer taborder = 30
boolean bringtotop = true
end type

on uo_selplantas.destroy
call uo_seleccion_plantas::destroy
end on

event ue_cambio;call super::ue_cambio;integer li_nula

setnull(li_nula)

IF IsNull(This.Codigo) THEN RETURN

CHOOSE CASE This.Codigo
	CASE -1, -9
//		uo_camara.Todos(True)
//		uo_camara.cbx_Todos.Enabled	=	False
		
	CASE ELSE
//		uo_camara.Filtra(This.Codigo)
//		uo_camara.cbx_Todos.Enabled	=	True
		dw_camara.SetItem(1, "cama_codigo", li_Nula)
		idwc_camara.Retrieve(This.codigo)
END CHOOSE
end event

type uo_selcliente from uo_seleccion_clientesprod within w_info_consultacamarapallet
integer x = 681
integer y = 536
integer width = 974
integer taborder = 40
boolean bringtotop = true
end type

on uo_selcliente.destroy
call uo_seleccion_clientesprod::destroy
end on

type st_1 from statictext within w_info_consultacamarapallet
integer x = 366
integer y = 640
integer width = 251
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Cliente"
boolean focusrectangle = false
end type

type st_2 from statictext within w_info_consultacamarapallet
integer x = 366
integer y = 828
integer width = 251
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Planta"
boolean focusrectangle = false
end type

type st_6 from statictext within w_info_consultacamarapallet
integer x = 366
integer y = 992
integer width = 251
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Cámara"
boolean focusrectangle = false
end type

type dw_camara from datawindow within w_info_consultacamarapallet
integer x = 663
integer y = 968
integer width = 910
integer height = 120
integer taborder = 40
boolean bringtotop = true
string title = "none"
string dataobject = "dddw_camarasbode"
boolean border = false
boolean livescroll = true
end type

event itemchanged;integer li_count ,li_codigo,li_nula

setnull(li_nula)

li_codigo=integer(data)

select count(*) into :li_count
  from dbo.camarasbode
 where plde_codigo=:uo_SelPlantas.codigo
   and cama_codigo=:li_codigo
 using sqlca;

If li_count=0 Then
	This.SetItem(1, "cama_codigo", li_Nula)
	Messagebox('Atención','Cámara no existe para la Planta Seleccionada')
   	Return 1
End If
end event

type st_7 from statictext within w_info_consultacamarapallet
integer x = 251
integer y = 440
integer width = 1655
integer height = 780
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 16711680
boolean enabled = false
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type


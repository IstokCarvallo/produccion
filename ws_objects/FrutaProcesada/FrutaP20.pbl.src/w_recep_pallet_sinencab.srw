$PBExportHeader$w_recep_pallet_sinencab.srw
$PBExportComments$Muestra los pallet, con tipo de entrasa desde Packing,  que no han sido asimilados a ninún encabezado de recepción.
forward
global type w_recep_pallet_sinencab from w_para_informes
end type
type st_1 from statictext within w_recep_pallet_sinencab
end type
type st_2 from statictext within w_recep_pallet_sinencab
end type
type st_3 from statictext within w_recep_pallet_sinencab
end type
type uo_selplantas from uo_seleccion_plantas within w_recep_pallet_sinencab
end type
type uo_selcliente from uo_seleccion_clientesprod within w_recep_pallet_sinencab
end type
end forward

global type w_recep_pallet_sinencab from w_para_informes
integer width = 2331
integer height = 1212
st_1 st_1
st_2 st_2
st_3 st_3
uo_selplantas uo_selplantas
uo_selcliente uo_selcliente
end type
global w_recep_pallet_sinencab w_recep_pallet_sinencab

type variables


end variables

on w_recep_pallet_sinencab.create
int iCurrent
call super::create
this.st_1=create st_1
this.st_2=create st_2
this.st_3=create st_3
this.uo_selplantas=create uo_selplantas
this.uo_selcliente=create uo_selcliente
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.st_1
this.Control[iCurrent+2]=this.st_2
this.Control[iCurrent+3]=this.st_3
this.Control[iCurrent+4]=this.uo_selplantas
this.Control[iCurrent+5]=this.uo_selcliente
end on

on w_recep_pallet_sinencab.destroy
call super::destroy
destroy(this.st_1)
destroy(this.st_2)
destroy(this.st_3)
destroy(this.uo_selplantas)
destroy(this.uo_selcliente)
end on

event open;call super::open;Boolean lb_Cerrar 

If IsNull(uo_SelPlantas.Codigo) Then lb_Cerrar = True
If IsNull(uo_SelCliente.Codigo) Then lb_Cerrar = True

If lb_Cerrar Then
	Close(This)
Else
	uo_SelPlantas.Seleccion(True, False)
	uo_SelCliente.Seleccion(False, False)
	uo_SelCliente.Inicia(gi_CodExport)
	uo_SelPlantas.Filtra(1)
End If
end event

type pb_excel from w_para_informes`pb_excel within w_recep_pallet_sinencab
integer x = 1797
integer y = 84
end type

type st_computador from w_para_informes`st_computador within w_recep_pallet_sinencab
end type

type st_usuario from w_para_informes`st_usuario within w_recep_pallet_sinencab
end type

type st_temporada from w_para_informes`st_temporada within w_recep_pallet_sinencab
end type

type p_logo from w_para_informes`p_logo within w_recep_pallet_sinencab
end type

type st_titulo from w_para_informes`st_titulo within w_recep_pallet_sinencab
integer width = 1463
string text = "Informe de Pallets sin Encabezado"
end type

type pb_acepta from w_para_informes`pb_acepta within w_recep_pallet_sinencab
integer x = 1819
integer y = 388
integer taborder = 40
end type

event pb_acepta::clicked;SetPointer(Arrow!)

Integer	fila

istr_info.titulo	= 'INFORME DE PALLETS SIN ENCABEZADO'

OpenWithParm(vinf, istr_info)
vinf.dw_1.DataObject = 'dw_info_recepcion_pallet_sinencab'
vinf.dw_1.SetTransObject(sqlca)
fila	=	vinf.dw_1.Retrieve(uo_SelCliente.Codigo, uo_SelPlantas.Codigo)

IF fila = -1 THEN
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
			   	"de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
ELSEIF fila = 0 THEN
	MessageBox( "No Existe información", "No existe información para este informe.", StopSign!, Ok!)
ELSE
	F_Membrete(vinf.dw_1)
	IF gs_Ambiente <> 'Windows' THEN F_ImprimeInformePdf(vinf.dw_1, istr_info.titulo)
END IF

SetPointer(Arrow!)
end event

type pb_salir from w_para_informes`pb_salir within w_recep_pallet_sinencab
integer x = 1819
integer y = 684
integer taborder = 50
end type

type st_1 from statictext within w_recep_pallet_sinencab
integer x = 251
integer y = 440
integer width = 1463
integer height = 504
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 16711680
alignment alignment = center!
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type st_2 from statictext within w_recep_pallet_sinencab
integer x = 389
integer y = 524
integer width = 215
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 16777215
long backcolor = 553648127
string text = "Cliente"
boolean focusrectangle = false
end type

type st_3 from statictext within w_recep_pallet_sinencab
integer x = 389
integer y = 784
integer width = 197
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 16777215
long backcolor = 553648127
string text = "Planta"
boolean focusrectangle = false
end type

type uo_selplantas from uo_seleccion_plantas within w_recep_pallet_sinencab
event destroy ( )
integer x = 622
integer y = 688
integer taborder = 40
boolean bringtotop = true
end type

on uo_selplantas.destroy
call uo_seleccion_plantas::destroy
end on

type uo_selcliente from uo_seleccion_clientesprod within w_recep_pallet_sinencab
event destroy ( )
integer x = 622
integer y = 516
integer height = 100
integer taborder = 20
boolean bringtotop = true
end type

on uo_selcliente.destroy
call uo_seleccion_clientesprod::destroy
end on


$PBExportHeader$w_info_archivos_pendientes.srw
$PBExportComments$Informe Consolidado General de Embarques.
forward
global type w_info_archivos_pendientes from w_para_informes
end type
type st_4 from statictext within w_info_archivos_pendientes
end type
type st_3 from statictext within w_info_archivos_pendientes
end type
type st_1 from statictext within w_info_archivos_pendientes
end type
type em_desde from editmask within w_info_archivos_pendientes
end type
type st_2 from statictext within w_info_archivos_pendientes
end type
type em_hasta from editmask within w_info_archivos_pendientes
end type
type st_5 from statictext within w_info_archivos_pendientes
end type
type cbx_generados from checkbox within w_info_archivos_pendientes
end type
type uo_selcliente from uo_seleccion_clientesprod within w_info_archivos_pendientes
end type
type uo_SelPlantas from uo_seleccion_plantas within w_info_archivos_pendientes
end type
end forward

global type w_info_archivos_pendientes from w_para_informes
integer width = 2875
integer height = 1232
event ue_listo ( )
st_4 st_4
st_3 st_3
st_1 st_1
em_desde em_desde
st_2 st_2
em_hasta em_hasta
st_5 st_5
cbx_generados cbx_generados
uo_selcliente uo_selcliente
uo_SelPlantas uo_SelPlantas
end type
global w_info_archivos_pendientes w_info_archivos_pendientes

type variables


end variables

event ue_listo();w_main.SetMicroHelp("Listo")
SetPointer(Arrow!)

end event

on w_info_archivos_pendientes.create
int iCurrent
call super::create
this.st_4=create st_4
this.st_3=create st_3
this.st_1=create st_1
this.em_desde=create em_desde
this.st_2=create st_2
this.em_hasta=create em_hasta
this.st_5=create st_5
this.cbx_generados=create cbx_generados
this.uo_selcliente=create uo_selcliente
this.uo_SelPlantas=create uo_SelPlantas
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.st_4
this.Control[iCurrent+2]=this.st_3
this.Control[iCurrent+3]=this.st_1
this.Control[iCurrent+4]=this.em_desde
this.Control[iCurrent+5]=this.st_2
this.Control[iCurrent+6]=this.em_hasta
this.Control[iCurrent+7]=this.st_5
this.Control[iCurrent+8]=this.cbx_generados
this.Control[iCurrent+9]=this.uo_selcliente
this.Control[iCurrent+10]=this.uo_SelPlantas
end on

on w_info_archivos_pendientes.destroy
call super::destroy
destroy(this.st_4)
destroy(this.st_3)
destroy(this.st_1)
destroy(this.em_desde)
destroy(this.st_2)
destroy(this.em_hasta)
destroy(this.st_5)
destroy(this.cbx_generados)
destroy(this.uo_selcliente)
destroy(this.uo_SelPlantas)
end on

event open;call super::open;Boolean	lb_Cerrar 

If IsNull(uo_SelCliente.Codigo) Then lb_Cerrar = True
If IsNull(uo_SelPlantas.Codigo) Then lb_Cerrar = True

If lb_Cerrar Then
	Close(This)
Else
	uo_SelCliente.Seleccion(False, False)
	uo_SelPlantas.Seleccion(False, False)
	uo_SelCliente.Inicia(gi_CodExport)
	uo_SelPlantas.Inicia(gi_codplanta)
	
	em_desde.Text				=	String(RelativeDate(today(), -365)) +' '+String(now())
	em_hasta.Text				=	String(Today())+' '+String(now())
End If
end event

type pb_excel from w_para_informes`pb_excel within w_info_archivos_pendientes
end type

type st_computador from w_para_informes`st_computador within w_info_archivos_pendientes
end type

type st_usuario from w_para_informes`st_usuario within w_info_archivos_pendientes
end type

type st_temporada from w_para_informes`st_temporada within w_info_archivos_pendientes
end type

type p_logo from w_para_informes`p_logo within w_info_archivos_pendientes
end type

type st_titulo from w_para_informes`st_titulo within w_info_archivos_pendientes
integer x = 247
integer width = 1957
string text = "Estado de Generación de Archivo Puerto"
end type

type pb_acepta from w_para_informes`pb_acepta within w_info_archivos_pendientes
string tag = "Imprimir Reporte"
integer x = 2382
integer y = 476
integer taborder = 110
end type

event pb_acepta::clicked;SetPointer(Arrow!)

Integer		fila, li_incluye = 1

IF cbx_generados.Checked THEN li_incluye = -1

istr_info.titulo	= 'ARCHIVOS GENERADOS PUERTO'

OpenWithParm(vinf, istr_info)
vinf.dw_1.DataObject = "dw_info_archivosnogeneradossaam"
vinf.dw_1.SetTransObject(sqlca)

fila	=	vinf.dw_1.Retrieve(uo_SelCliente.Codigo, uo_SelPlantas.Codigo, DateTime(em_desde.Text), DateTime(em_hasta.Text), li_incluye)

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

type pb_salir from w_para_informes`pb_salir within w_info_archivos_pendientes
string tag = "Salir [Cerrar Ventana Activa]"
integer x = 2382
integer y = 764
integer taborder = 120
string powertiptext = "Salir [Cerrar Ventana Activa]"
end type

type st_4 from statictext within w_info_archivos_pendientes
integer x = 343
integer y = 628
integer width = 229
integer height = 64
integer taborder = 40
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

type st_3 from statictext within w_info_archivos_pendientes
integer x = 343
integer y = 488
integer width = 229
integer height = 64
integer taborder = 20
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

type st_1 from statictext within w_info_archivos_pendientes
integer x = 247
integer y = 408
integer width = 1957
integer height = 616
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 16711680
boolean enabled = false
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type em_desde from editmask within w_info_archivos_pendientes
integer x = 581
integer y = 760
integer width = 645
integer height = 100
integer taborder = 20
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long backcolor = 16777215
alignment alignment = center!
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = datetimemask!
string mask = "dd/mm/yyyy hh:mm"
end type

type st_2 from statictext within w_info_archivos_pendientes
integer x = 343
integer y = 780
integer width = 229
integer height = 64
integer taborder = 40
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Desde"
boolean focusrectangle = false
end type

type em_hasta from editmask within w_info_archivos_pendientes
integer x = 1495
integer y = 764
integer width = 645
integer height = 100
integer taborder = 30
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long backcolor = 16777215
alignment alignment = center!
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = datetimemask!
string mask = "dd/mm/yyyy hh:mm"
end type

type st_5 from statictext within w_info_archivos_pendientes
integer x = 1262
integer y = 780
integer width = 229
integer height = 60
integer taborder = 40
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Hasta"
boolean focusrectangle = false
end type

type cbx_generados from checkbox within w_info_archivos_pendientes
integer x = 827
integer y = 912
integer width = 937
integer height = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Incluye Archivos Generados"
end type

event clicked;IF This.Checked THEN
	em_desde.Enabled = False
	em_hasta.Enabled = False
ELSE	
	em_desde.Enabled = True
	em_hasta.Enabled = True
END IF	
end event

type uo_selcliente from uo_seleccion_clientesprod within w_info_archivos_pendientes
event destroy ( )
integer x = 581
integer y = 476
integer height = 92
integer taborder = 40
boolean bringtotop = true
end type

on uo_selcliente.destroy
call uo_seleccion_clientesprod::destroy
end on

type uo_SelPlantas from uo_seleccion_plantas within w_info_archivos_pendientes
event destroy ( )
integer x = 581
integer y = 616
integer height = 92
integer taborder = 30
boolean bringtotop = true
end type

on uo_SelPlantas.destroy
call uo_seleccion_plantas::destroy
end on


$PBExportHeader$w_info_frucomercial_proforma.srw
$PBExportComments$Seleccion de Datos de Tarifa Fruta Comercial
forward
global type w_info_frucomercial_proforma from w_para_informes
end type
type st_4 from statictext within w_info_frucomercial_proforma
end type
type st_5 from statictext within w_info_frucomercial_proforma
end type
type em_fecha from editmask within w_info_frucomercial_proforma
end type
type st_3 from statictext within w_info_frucomercial_proforma
end type
type st_1 from statictext within w_info_frucomercial_proforma
end type
type cbx_informe from checkbox within w_info_frucomercial_proforma
end type
type uo_selcliente from uo_seleccion_clientesprod within w_info_frucomercial_proforma
end type
type uo_selproductor from uo_seleccion_productor within w_info_frucomercial_proforma
end type
type uo_selespecie from uo_seleccion_especie within w_info_frucomercial_proforma
end type
type st_2 from statictext within w_info_frucomercial_proforma
end type
end forward

global type w_info_frucomercial_proforma from w_para_informes
integer x = 0
integer y = 0
integer width = 2409
integer height = 1448
st_4 st_4
st_5 st_5
em_fecha em_fecha
st_3 st_3
st_1 st_1
cbx_informe cbx_informe
uo_selcliente uo_selcliente
uo_selproductor uo_selproductor
uo_selespecie uo_selespecie
st_2 st_2
end type
global w_info_frucomercial_proforma w_info_frucomercial_proforma

type variables


end variables

forward prototypes
public subroutine consolidalote ()
end prototypes

public subroutine consolidalote ();
end subroutine

on w_info_frucomercial_proforma.create
int iCurrent
call super::create
this.st_4=create st_4
this.st_5=create st_5
this.em_fecha=create em_fecha
this.st_3=create st_3
this.st_1=create st_1
this.cbx_informe=create cbx_informe
this.uo_selcliente=create uo_selcliente
this.uo_selproductor=create uo_selproductor
this.uo_selespecie=create uo_selespecie
this.st_2=create st_2
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.st_4
this.Control[iCurrent+2]=this.st_5
this.Control[iCurrent+3]=this.em_fecha
this.Control[iCurrent+4]=this.st_3
this.Control[iCurrent+5]=this.st_1
this.Control[iCurrent+6]=this.cbx_informe
this.Control[iCurrent+7]=this.uo_selcliente
this.Control[iCurrent+8]=this.uo_selproductor
this.Control[iCurrent+9]=this.uo_selespecie
this.Control[iCurrent+10]=this.st_2
end on

on w_info_frucomercial_proforma.destroy
call super::destroy
destroy(this.st_4)
destroy(this.st_5)
destroy(this.em_fecha)
destroy(this.st_3)
destroy(this.st_1)
destroy(this.cbx_informe)
destroy(this.uo_selcliente)
destroy(this.uo_selproductor)
destroy(this.uo_selespecie)
destroy(this.st_2)
end on

event open;call super::open;Boolean	lb_Cerrar

IF IsNull(uo_SelEspecie.Codigo) THEN lb_Cerrar	=	True
IF IsNull(uo_SelProductor.Codigo) THEN lb_Cerrar	=	True
IF IsNull(uo_SelCliente.Codigo) THEN lb_Cerrar	=	True

IF lb_Cerrar THEN
	Close(This)
ELSE
	uo_SelEspecie.Seleccion(True, False)
	uo_SelProductor.Seleccion(True, False)
	uo_SelCliente.Seleccion(False, False)
	uo_SelProductor.Filtra(-1)
	uo_SelCliente.Inicia(gi_CodExport)

	em_fecha.text = String(Today())
END IF
end event

type pb_excel from w_para_informes`pb_excel within w_info_frucomercial_proforma
end type

type st_computador from w_para_informes`st_computador within w_info_frucomercial_proforma
integer x = 997
integer width = 1234
end type

type st_usuario from w_para_informes`st_usuario within w_info_frucomercial_proforma
integer x = 997
integer width = 1234
end type

type st_temporada from w_para_informes`st_temporada within w_info_frucomercial_proforma
integer x = 997
integer width = 1234
end type

type p_logo from w_para_informes`p_logo within w_info_frucomercial_proforma
end type

type st_titulo from w_para_informes`st_titulo within w_info_frucomercial_proforma
integer width = 1568
string text = "Informe Factura Proforma"
end type

type pb_acepta from w_para_informes`pb_acepta within w_info_frucomercial_proforma
integer x = 1957
integer y = 516
integer taborder = 50
integer textsize = -8
fontcharset fontcharset = ansi!
end type

event pb_acepta::clicked;SetPointer(HourGlass!)
Date     	ld_fecha, ld_Hasta
Long		ll_Fila

istr_info.titulo	= "FACTURA PROFORMA FRUTA COMERCIAL"
istr_info.copias	= 1
	
OpenWithParm(vinf, istr_info)

If cbx_informe.Checked Then
	vinf.dw_1.DataObject = "dw_info_revision_proforma_comer"
Else
	vinf.dw_1.DataObject = "dw_info_fproforma_fcomer"
End If

vinf.dw_1.SetTransObject(sqlca)
ld_fecha = Date('01/'+ em_fecha.text)

If ld_fecha = Date(01/01/1900) Then
	Messagebox("Atención","Debe ingresar mes de proceso")
	Return 1
End If

ll_Fila	=	vinf.dw_1.Retrieve(uo_SelProductor.Codigo, ld_fecha, uo_SelEspecie.Codigo, uo_SelCliente.Codigo)

If ll_Fila = -1 Then
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
					"de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)	
ElseIf ll_Fila = 0 Then
	MessageBox( "No Existe información", "No existe información para este informe.", StopSign!, Ok!)
Else
	F_Membrete(vinf.dw_1)	
	//vinf.dw_1.Object.DataWindow.Zoom = 95
	If gs_Ambiente <> 'Windows' Then F_ImprimeInformePdf(vinf.dw_1, istr_info.titulo)
End If

SetPointer(Arrow!)
end event

type pb_salir from w_para_informes`pb_salir within w_info_frucomercial_proforma
integer x = 1957
integer y = 880
integer taborder = 60
integer textsize = -8
fontcharset fontcharset = ansi!
end type

type st_4 from statictext within w_info_frucomercial_proforma
integer x = 311
integer y = 876
integer width = 297
integer height = 64
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Especie"
boolean focusrectangle = false
end type

type st_5 from statictext within w_info_frucomercial_proforma
integer x = 311
integer y = 668
integer width = 297
integer height = 64
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Productor"
boolean focusrectangle = false
end type

type em_fecha from editmask within w_info_frucomercial_proforma
integer x = 686
integer y = 984
integer width = 375
integer height = 84
integer taborder = 40
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 33554432
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = datemask!
string mask = "mm/yyyy"
boolean dropdowncalendar = true
end type

type st_3 from statictext within w_info_frucomercial_proforma
integer x = 311
integer y = 488
integer width = 229
integer height = 64
boolean bringtotop = true
integer textsize = -9
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

type st_1 from statictext within w_info_frucomercial_proforma
integer x = 247
integer y = 424
integer width = 1568
integer height = 876
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

type cbx_informe from checkbox within w_info_frucomercial_proforma
integer x = 686
integer y = 1140
integer width = 805
integer height = 76
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Emite Informe Revisión"
end type

type uo_selcliente from uo_seleccion_clientesprod within w_info_frucomercial_proforma
integer x = 686
integer y = 476
integer height = 88
integer taborder = 80
boolean bringtotop = true
end type

on uo_selcliente.destroy
call uo_seleccion_clientesprod::destroy
end on

type uo_selproductor from uo_seleccion_productor within w_info_frucomercial_proforma
integer x = 686
integer y = 584
integer taborder = 80
boolean bringtotop = true
end type

on uo_selproductor.destroy
call uo_seleccion_productor::destroy
end on

type uo_selespecie from uo_seleccion_especie within w_info_frucomercial_proforma
integer x = 686
integer y = 780
integer taborder = 80
boolean bringtotop = true
end type

on uo_selespecie.destroy
call uo_seleccion_especie::destroy
end on

type st_2 from statictext within w_info_frucomercial_proforma
integer x = 311
integer y = 1000
integer width = 361
integer height = 64
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Mes Proceso"
boolean focusrectangle = false
end type


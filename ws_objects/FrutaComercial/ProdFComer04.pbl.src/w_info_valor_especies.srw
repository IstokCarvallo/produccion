$PBExportHeader$w_info_valor_especies.srw
$PBExportComments$Informe del Valor de las Especies junto con su detalle
forward
global type w_info_valor_especies from w_para_informes
end type
type uo_selespe from uo_seleccion_especie within w_info_valor_especies
end type
type uo_selvari from uo_seleccion_variedad within w_info_valor_especies
end type
type uo_selplta from uo_seleccion_plantas within w_info_valor_especies
end type
type uo_selcama from uo_seleccion_camara within w_info_valor_especies
end type
type uo_selcate from uo_seleccion_categoria within w_info_valor_especies
end type
type em_fecha from editmask within w_info_valor_especies
end type
type uo_selexpo from uo_seleccion_clientesprod within w_info_valor_especies
end type
type st_3 from statictext within w_info_valor_especies
end type
type st_6 from statictext within w_info_valor_especies
end type
type st_7 from statictext within w_info_valor_especies
end type
type st_2 from statictext within w_info_valor_especies
end type
type st_4 from statictext within w_info_valor_especies
end type
type st_5 from statictext within w_info_valor_especies
end type
type st_9 from statictext within w_info_valor_especies
end type
type st_1 from statictext within w_info_valor_especies
end type
type st_10 from statictext within w_info_valor_especies
end type
type st_12 from statictext within w_info_valor_especies
end type
type st_11 from statictext within w_info_valor_especies
end type
type st_13 from statictext within w_info_valor_especies
end type
end forward

global type w_info_valor_especies from w_para_informes
integer x = 0
integer y = 0
integer width = 3643
integer height = 1548
uo_selespe uo_selespe
uo_selvari uo_selvari
uo_selplta uo_selplta
uo_selcama uo_selcama
uo_selcate uo_selcate
em_fecha em_fecha
uo_selexpo uo_selexpo
st_3 st_3
st_6 st_6
st_7 st_7
st_2 st_2
st_4 st_4
st_5 st_5
st_9 st_9
st_1 st_1
st_10 st_10
st_12 st_12
st_11 st_11
st_13 st_13
end type
global w_info_valor_especies w_info_valor_especies

forward prototypes
public subroutine consolidalote ()
end prototypes

public subroutine consolidalote ();IF uo_SelEspe.Codigo = -9 OR &
	uo_SelVari.Codigo = -9 THEN
ELSE

END IF

RETURN
end subroutine

on w_info_valor_especies.create
int iCurrent
call super::create
this.uo_selespe=create uo_selespe
this.uo_selvari=create uo_selvari
this.uo_selplta=create uo_selplta
this.uo_selcama=create uo_selcama
this.uo_selcate=create uo_selcate
this.em_fecha=create em_fecha
this.uo_selexpo=create uo_selexpo
this.st_3=create st_3
this.st_6=create st_6
this.st_7=create st_7
this.st_2=create st_2
this.st_4=create st_4
this.st_5=create st_5
this.st_9=create st_9
this.st_1=create st_1
this.st_10=create st_10
this.st_12=create st_12
this.st_11=create st_11
this.st_13=create st_13
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.uo_selespe
this.Control[iCurrent+2]=this.uo_selvari
this.Control[iCurrent+3]=this.uo_selplta
this.Control[iCurrent+4]=this.uo_selcama
this.Control[iCurrent+5]=this.uo_selcate
this.Control[iCurrent+6]=this.em_fecha
this.Control[iCurrent+7]=this.uo_selexpo
this.Control[iCurrent+8]=this.st_3
this.Control[iCurrent+9]=this.st_6
this.Control[iCurrent+10]=this.st_7
this.Control[iCurrent+11]=this.st_2
this.Control[iCurrent+12]=this.st_4
this.Control[iCurrent+13]=this.st_5
this.Control[iCurrent+14]=this.st_9
this.Control[iCurrent+15]=this.st_1
this.Control[iCurrent+16]=this.st_10
this.Control[iCurrent+17]=this.st_12
this.Control[iCurrent+18]=this.st_11
this.Control[iCurrent+19]=this.st_13
end on

on w_info_valor_especies.destroy
call super::destroy
destroy(this.uo_selespe)
destroy(this.uo_selvari)
destroy(this.uo_selplta)
destroy(this.uo_selcama)
destroy(this.uo_selcate)
destroy(this.em_fecha)
destroy(this.uo_selexpo)
destroy(this.st_3)
destroy(this.st_6)
destroy(this.st_7)
destroy(this.st_2)
destroy(this.st_4)
destroy(this.st_5)
destroy(this.st_9)
destroy(this.st_1)
destroy(this.st_10)
destroy(this.st_12)
destroy(this.st_11)
destroy(this.st_13)
end on

event open;call super::open;Boolean	lb_Cerrar

IF IsNull(uo_SelExpo.Codigo) THEN lb_Cerrar	=	True

IF IsNull(uo_SelPlta.Codigo) THEN lb_Cerrar	=	True

IF IsNull(uo_SelCama.Codigo) THEN lb_Cerrar	=	True

IF IsNull(uo_SelEspe.Codigo) THEN lb_Cerrar	=	True

IF IsNull(uo_SelVari.Codigo) THEN lb_Cerrar	=	True

IF IsNull(uo_SelCate.Codigo) THEN lb_Cerrar	=	True

IF lb_Cerrar THEN
	Close(This)
ELSE
	uo_SelExpo.Seleccion(True, False)
	uo_SelPlta.Seleccion(True, False)
	uo_SelCama.Seleccion(True, False)
	uo_SelEspe.Seleccion(True, False)
	uo_SelVari.Seleccion(True, False)
	uo_SelCate.Seleccion(True, False)
	
	uo_SelVari.Todos(True)
	
	uo_SelVari.cbx_Todos.Enabled	=	False

	uo_SelCama.Todos(True)
	
	uo_SelCama.cbx_Todos.Enabled	=	False
END IF

em_fecha.text = String(Today(),"dd/mm/yyyy")
end event

type pb_excel from w_para_informes`pb_excel within w_info_valor_especies
end type

type st_computador from w_para_informes`st_computador within w_info_valor_especies
integer x = 1024
integer width = 2583
end type

type st_usuario from w_para_informes`st_usuario within w_info_valor_especies
integer x = 1024
integer width = 2583
end type

type st_temporada from w_para_informes`st_temporada within w_info_valor_especies
integer x = 1024
integer width = 2583
end type

type p_logo from w_para_informes`p_logo within w_info_valor_especies
end type

type st_titulo from w_para_informes`st_titulo within w_info_valor_especies
integer width = 2930
fontcharset fontcharset = ansi!
string text = "Informe Valor de las Especies"
end type

type pb_acepta from w_para_informes`pb_acepta within w_info_valor_especies
integer x = 3264
integer y = 552
integer taborder = 120
integer textsize = -8
fontcharset fontcharset = ansi!
end type

event pb_acepta::clicked;Date ld_fecha

SetPointer(HourGlass!)

Long		ll_Fila
Integer	li_Ordenamiento, li_ConsolidaEspecie, li_ConsolidaVariedad, &
			li_ConsolidaLote, li_Detalle

istr_info.titulo	= "INFORME DE TARIFAS DE LAS ESPECIES"
istr_info.copias	= 1
	
OpenWithParm(vinf, istr_info)

vinf.dw_1.DataObject = "dw_info_valor_especies"

vinf.dw_1.SetTransObject(sqlca)


IF uo_SelEspe.Codigo = -9 THEN
	uo_SelEspe.Codigo 	= -1
	li_ConsolidaEspecie	=	1
ELSE
	li_ConsolidaEspecie	=	0
END IF

IF uo_SelVari.Codigo = -9 THEN
	uo_SelVari.Codigo 	= -1
	li_ConsolidaVariedad	=	1
ELSE
	li_ConsolidaVariedad	=	0
END IF
 
ld_fecha = date(em_fecha.text) 

IF IsNull(ld_fecha) OR ld_fecha = Date("00/00/0000") THEN
	Messagebox("Error de Consistencia","Ingrese una Fecha Correcta")
	Return 1
END IF
 
ll_Fila	=	vinf.dw_1.Retrieve(uo_SelExpo.Codigo, uo_SelEspe.Codigo, & 
										 uo_SelVari.Codigo, uo_SelPlta.Codigo, &
										 uo_SelCama.Codigo, uo_SelCate.Codigo, &
										 Date(ld_fecha))

IF ll_Fila = -1 THEN
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
					"de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
ELSEIF ll_Fila = 0 THEN
	MessageBox( "No Existe información", "No existe información para este informe.", &
					StopSign!, Ok!)
ELSE
	F_Membrete(vinf.dw_1)	
	IF gs_Ambiente <> 'Windows' THEN
	   	F_ImprimeInformePdf(vinf.dw_1, istr_info.titulo)
	
  	END IF

END IF	

SetPointer(Arrow!)
end event

type pb_salir from w_para_informes`pb_salir within w_info_valor_especies
integer x = 3264
integer y = 916
integer taborder = 130
integer textsize = -8
fontcharset fontcharset = ansi!
end type

type uo_selespe from uo_seleccion_especie within w_info_valor_especies
integer x = 2217
integer y = 732
integer taborder = 50
boolean bringtotop = true
end type

on uo_selespe.destroy
call uo_seleccion_especie::destroy
end on

event ue_cambio();IF IsNull(This.Codigo) THEN RETURN

CHOOSE CASE This.Codigo
	CASE -1, -9
		uo_SelVari.Todos(True)
		
		uo_SelVari.cbx_Todos.Enabled	=	False
		
	CASE ELSE
		uo_SelVari.Filtra(This.Codigo)
		
		uo_SelVari.cbx_Todos.Enabled	=	True
		
END CHOOSE

ConsolidaLote()


end event

type uo_selvari from uo_seleccion_variedad within w_info_valor_especies
integer x = 2217
integer y = 928
integer taborder = 60
boolean bringtotop = true
end type

on uo_selvari.destroy
call uo_seleccion_variedad::destroy
end on

event ue_cambio();call super::ue_cambio;ConsolidaLote()
end event

type uo_selplta from uo_seleccion_plantas within w_info_valor_especies
integer x = 677
integer y = 732
integer taborder = 30
boolean bringtotop = true
end type

on uo_selplta.destroy
call uo_seleccion_plantas::destroy
end on

event ue_cambio();call super::ue_cambio;IF IsNull(This.Codigo) THEN RETURN

CHOOSE CASE This.Codigo
	CASE -1, -9
		uo_SelCama.Todos(True)
		
		uo_SelCama.cbx_Todos.Enabled	=	False
		
	CASE ELSE
		uo_SelCama.Filtra(This.Codigo)
		
		uo_SelCama.cbx_Todos.Enabled	=	True
		
END CHOOSE

ConsolidaLote()
end event

type uo_selcama from uo_seleccion_camara within w_info_valor_especies
integer x = 677
integer y = 928
integer taborder = 40
boolean bringtotop = true
end type

on uo_selcama.destroy
call uo_seleccion_camara::destroy
end on

type uo_selcate from uo_seleccion_categoria within w_info_valor_especies
integer x = 2217
integer y = 468
integer taborder = 80
boolean bringtotop = true
end type

on uo_selcate.destroy
call uo_seleccion_categoria::destroy
end on

type em_fecha from editmask within w_info_valor_especies
integer x = 677
integer y = 1188
integer width = 293
integer height = 80
integer taborder = 70
boolean bringtotop = true
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
string text = "none"
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = datemask!
string mask = "dd/mm/yyyy"
end type

type uo_selexpo from uo_seleccion_clientesprod within w_info_valor_especies
integer x = 677
integer y = 464
integer taborder = 130
boolean bringtotop = true
end type

on uo_selexpo.destroy
call uo_seleccion_clientesprod::destroy
end on

type st_3 from statictext within w_info_valor_especies
integer x = 315
integer y = 468
integer width = 338
integer height = 64
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 16777215
long backcolor = 553648127
string text = "Exportador"
boolean focusrectangle = false
end type

type st_6 from statictext within w_info_valor_especies
integer x = 315
integer y = 732
integer width = 274
integer height = 64
integer textsize = -8
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

type st_7 from statictext within w_info_valor_especies
integer x = 315
integer y = 928
integer width = 270
integer height = 64
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 16777215
long backcolor = 553648127
string text = "Cámara"
boolean focusrectangle = false
end type

type st_2 from statictext within w_info_valor_especies
integer x = 315
integer y = 1196
integer width = 302
integer height = 64
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 16777215
long backcolor = 553648127
string text = "Fecha"
boolean focusrectangle = false
end type

type st_4 from statictext within w_info_valor_especies
integer x = 1851
integer y = 732
integer width = 320
integer height = 64
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 16777215
long backcolor = 553648127
string text = "Especie"
boolean focusrectangle = false
end type

type st_5 from statictext within w_info_valor_especies
integer x = 1851
integer y = 928
integer width = 279
integer height = 64
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 16777215
long backcolor = 553648127
string text = "Variedad"
boolean focusrectangle = false
end type

type st_9 from statictext within w_info_valor_especies
integer x = 1851
integer y = 468
integer width = 302
integer height = 64
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 16777215
long backcolor = 553648127
string text = "Categoria"
boolean focusrectangle = false
end type

type st_1 from statictext within w_info_valor_especies
integer x = 251
integer y = 436
integer width = 1568
integer height = 272
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

type st_10 from statictext within w_info_valor_especies
integer x = 251
integer y = 708
integer width = 1568
integer height = 432
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

type st_12 from statictext within w_info_valor_especies
integer x = 1819
integer y = 708
integer width = 1358
integer height = 432
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

type st_11 from statictext within w_info_valor_especies
integer x = 1819
integer y = 436
integer width = 1358
integer height = 272
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

type st_13 from statictext within w_info_valor_especies
integer x = 251
integer y = 1140
integer width = 2926
integer height = 176
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


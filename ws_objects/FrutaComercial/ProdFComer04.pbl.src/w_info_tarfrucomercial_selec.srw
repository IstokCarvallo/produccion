$PBExportHeader$w_info_tarfrucomercial_selec.srw
$PBExportComments$Seleccion de Datos de Tarifa Fruta Comercial
forward
global type w_info_tarfrucomercial_selec from w_para_informes
end type
type uo_selespe from uo_seleccion_especie within w_info_tarfrucomercial_selec
end type
type st_4 from statictext within w_info_tarfrucomercial_selec
end type
type st_5 from statictext within w_info_tarfrucomercial_selec
end type
type uo_selvari from uo_seleccion_variedad within w_info_tarfrucomercial_selec
end type
type em_fecha_desde from editmask within w_info_tarfrucomercial_selec
end type
type em_fecha_hasta from editmask within w_info_tarfrucomercial_selec
end type
type st_2 from statictext within w_info_tarfrucomercial_selec
end type
type st_3 from statictext within w_info_tarfrucomercial_selec
end type
type st_1 from statictext within w_info_tarfrucomercial_selec
end type
end forward

global type w_info_tarfrucomercial_selec from w_para_informes
integer x = 0
integer y = 0
integer width = 2341
integer height = 1224
uo_selespe uo_selespe
st_4 st_4
st_5 st_5
uo_selvari uo_selvari
em_fecha_desde em_fecha_desde
em_fecha_hasta em_fecha_hasta
st_2 st_2
st_3 st_3
st_1 st_1
end type
global w_info_tarfrucomercial_selec w_info_tarfrucomercial_selec

forward prototypes
public subroutine consolidalote ()
public subroutine parmtempo ()
end prototypes

public subroutine consolidalote ();
end subroutine

public subroutine parmtempo ();Date		 fech_ini, fech_ter

SELECT	pate_inicio, pate_termin
INTO		:fech_ini, :fech_ter
FROM		dba.paramtemporada
WHERE		pate_vigent = 1;

em_fecha_desde.text = String(fech_ini)
em_fecha_hasta.text = String(fech_ter)


end subroutine

on w_info_tarfrucomercial_selec.create
int iCurrent
call super::create
this.uo_selespe=create uo_selespe
this.st_4=create st_4
this.st_5=create st_5
this.uo_selvari=create uo_selvari
this.em_fecha_desde=create em_fecha_desde
this.em_fecha_hasta=create em_fecha_hasta
this.st_2=create st_2
this.st_3=create st_3
this.st_1=create st_1
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.uo_selespe
this.Control[iCurrent+2]=this.st_4
this.Control[iCurrent+3]=this.st_5
this.Control[iCurrent+4]=this.uo_selvari
this.Control[iCurrent+5]=this.em_fecha_desde
this.Control[iCurrent+6]=this.em_fecha_hasta
this.Control[iCurrent+7]=this.st_2
this.Control[iCurrent+8]=this.st_3
this.Control[iCurrent+9]=this.st_1
end on

on w_info_tarfrucomercial_selec.destroy
call super::destroy
destroy(this.uo_selespe)
destroy(this.st_4)
destroy(this.st_5)
destroy(this.uo_selvari)
destroy(this.em_fecha_desde)
destroy(this.em_fecha_hasta)
destroy(this.st_2)
destroy(this.st_3)
destroy(this.st_1)
end on

event open;call super::open;Boolean	lb_Cerrar


IF IsNull(uo_SelEspe.Codigo) THEN lb_Cerrar	=	True

IF IsNull(uo_SelVari.Codigo) THEN lb_Cerrar	=	True

IF lb_Cerrar THEN
	Close(This)
ELSE
	uo_SelEspe.Seleccion(True, False)
	uo_SelVari.Seleccion(True, False)

	uo_SelVari.Todos(True)
	
	uo_SelVari.cbx_Todos.Enabled	=	False
END IF

// Se Rescata las fechas de inicio y termino de temporada
parmtempo()
end event

type pb_excel from w_para_informes`pb_excel within w_info_tarfrucomercial_selec
end type

type st_computador from w_para_informes`st_computador within w_info_tarfrucomercial_selec
integer x = 1015
integer width = 1207
end type

type st_usuario from w_para_informes`st_usuario within w_info_tarfrucomercial_selec
integer x = 1015
integer width = 1207
end type

type st_temporada from w_para_informes`st_temporada within w_info_tarfrucomercial_selec
integer x = 1015
integer width = 1207
end type

type p_logo from w_para_informes`p_logo within w_info_tarfrucomercial_selec
end type

type st_titulo from w_para_informes`st_titulo within w_info_tarfrucomercial_selec
integer width = 1568
fontcharset fontcharset = ansi!
string text = "Informe Tarifa Fruta Comercial entre Fechas"
end type

type pb_acepta from w_para_informes`pb_acepta within w_info_tarfrucomercial_selec
integer x = 1966
integer y = 432
integer taborder = 120
integer textsize = -8
fontcharset fontcharset = ansi!
end type

event pb_acepta::clicked;Date ld_fechades, ld_fechahas
SetPointer(HourGlass!)

Long		ll_Fila
Integer	li_ConsolidaEspecie, li_ConsolidaVariedad, &
			li_ConsolidaLote

istr_info.titulo	= "TARIFA FRUTA COMERCIAL"
istr_info.copias	= 1
	
OpenWithParm(vinf, istr_info)

vinf.dw_1.DataObject = "dw_info_fecha_tarifafrutacomercial"

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

ld_fechades = Date(em_fecha_desde.text)
ld_fechahas = Date(em_fecha_hasta.text)


IF iSNull(ld_fechades) OR ld_fechades = date("00/00/000") OR &
	iSNull(ld_fechahas) OR ld_fechahas = date("00/00/0000") THEN
	Messagebox("Error de Consistencia","Error en el Ingreso de Fechas")
	Return 1
END IF

IF ld_fechades > ld_fechahas THEN
	Messagebox("Error de Consistencia", "Fecha desde tiene que ser mayor que Fecha Hasta")
	Return 1
END IF

ll_Fila	=	vinf.dw_1.Retrieve(uo_SelEspe.Codigo, uo_SelVari.Codigo, &
										Date(ld_fechades), Date(ld_fechahas))

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

type pb_salir from w_para_informes`pb_salir within w_info_tarfrucomercial_selec
integer x = 1961
integer y = 684
integer taborder = 130
integer textsize = -8
fontcharset fontcharset = ansi!
end type

type uo_selespe from uo_seleccion_especie within w_info_tarfrucomercial_selec
integer x = 763
integer y = 440
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


end event

type st_4 from statictext within w_info_tarfrucomercial_selec
integer x = 357
integer y = 524
integer width = 320
integer height = 64
boolean bringtotop = true
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

type st_5 from statictext within w_info_tarfrucomercial_selec
integer x = 357
integer y = 732
integer width = 320
integer height = 64
boolean bringtotop = true
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

type uo_selvari from uo_seleccion_variedad within w_info_tarfrucomercial_selec
integer x = 763
integer y = 636
integer height = 196
integer taborder = 60
boolean bringtotop = true
end type

on uo_selvari.destroy
call uo_seleccion_variedad::destroy
end on

type em_fecha_desde from editmask within w_info_tarfrucomercial_selec
integer x = 763
integer y = 852
integer width = 293
integer height = 84
integer taborder = 140
boolean bringtotop = true
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = datemask!
string mask = "dd/mm/yyyy"
end type

type em_fecha_hasta from editmask within w_info_tarfrucomercial_selec
integer x = 1339
integer y = 852
integer width = 293
integer height = 84
integer taborder = 150
boolean bringtotop = true
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = datemask!
string mask = "dd/mm/yyyy"
end type

type st_2 from statictext within w_info_tarfrucomercial_selec
integer x = 357
integer y = 860
integer width = 320
integer height = 64
boolean bringtotop = true
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 16777215
long backcolor = 553648127
string text = "Fecha Desde"
boolean focusrectangle = false
end type

type st_3 from statictext within w_info_tarfrucomercial_selec
integer x = 1147
integer y = 860
integer width = 197
integer height = 64
boolean bringtotop = true
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 16777215
long backcolor = 553648127
string text = "Hasta"
boolean focusrectangle = false
end type

type st_1 from statictext within w_info_tarfrucomercial_selec
integer x = 247
integer y = 424
integer width = 1568
integer height = 568
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


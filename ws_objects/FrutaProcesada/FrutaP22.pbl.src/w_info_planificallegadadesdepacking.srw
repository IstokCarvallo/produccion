$PBExportHeader$w_info_planificallegadadesdepacking.srw
$PBExportComments$Informe Existencia Sitio Revisión
forward
global type w_info_planificallegadadesdepacking from w_para_informes
end type
type st_1 from statictext within w_info_planificallegadadesdepacking
end type
type st_2 from statictext within w_info_planificallegadadesdepacking
end type
type em_desde from editmask within w_info_planificallegadadesdepacking
end type
type st_6 from statictext within w_info_planificallegadadesdepacking
end type
type st_3 from statictext within w_info_planificallegadadesdepacking
end type
type st_7 from statictext within w_info_planificallegadadesdepacking
end type
type em_hasta from editmask within w_info_planificallegadadesdepacking
end type
type st_variedad from statictext within w_info_planificallegadadesdepacking
end type
type st_5 from statictext within w_info_planificallegadadesdepacking
end type
type st_4 from statictext within w_info_planificallegadadesdepacking
end type
type uo_selespecie from uo_seleccion_especie within w_info_planificallegadadesdepacking
end type
type uo_selvariedad from uo_seleccion_variedad within w_info_planificallegadadesdepacking
end type
type st_8 from statictext within w_info_planificallegadadesdepacking
end type
type uo_selcliente from uo_seleccion_clientesprod within w_info_planificallegadadesdepacking
end type
type uo_selplanta from uo_seleccion_plantas within w_info_planificallegadadesdepacking
end type
type uo_SelPacking from uo_seleccion_plantas within w_info_planificallegadadesdepacking
end type
end forward

global type w_info_planificallegadadesdepacking from w_para_informes
integer x = 14
integer y = 32
integer width = 2501
integer height = 1612
string title = "PLANIFICA LLEGADA DESDE PACKING"
boolean minbox = false
boolean maxbox = false
boolean resizable = false
windowtype windowtype = response!
string icon = "F:\Desarrollo\Producción\FrutaProcesada\Producc.ico"
st_1 st_1
st_2 st_2
em_desde em_desde
st_6 st_6
st_3 st_3
st_7 st_7
em_hasta em_hasta
st_variedad st_variedad
st_5 st_5
st_4 st_4
uo_selespecie uo_selespecie
uo_selvariedad uo_selvariedad
st_8 st_8
uo_selcliente uo_selcliente
uo_selplanta uo_selplanta
uo_SelPacking uo_SelPacking
end type
global w_info_planificallegadadesdepacking w_info_planificallegadadesdepacking

type variables

end variables

forward prototypes
public function boolean existepacking (integer li_planta)
end prototypes

public function boolean existepacking (integer li_planta);Integer  li_codigo

SELECT	plde_codigo
INTO    :li_Codigo
FROM	dbo.plantadesp
WHERE	plde_codigo =  :li_planta;
	
IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura de tabla Planta")
	
	RETURN False
ELSEIF sqlca.SQLCode = 100 THEN
	MessageBox("Atención", "Código de Planta no ha sido Definido.~r~r" + &
					"Ingrese o seleccione otro Código.")
	RETURN False
ELSE
	//istr_mant.argumento[7] = String(li_planta)
	RETURN True 
END IF
end function

on w_info_planificallegadadesdepacking.create
int iCurrent
call super::create
this.st_1=create st_1
this.st_2=create st_2
this.em_desde=create em_desde
this.st_6=create st_6
this.st_3=create st_3
this.st_7=create st_7
this.em_hasta=create em_hasta
this.st_variedad=create st_variedad
this.st_5=create st_5
this.st_4=create st_4
this.uo_selespecie=create uo_selespecie
this.uo_selvariedad=create uo_selvariedad
this.st_8=create st_8
this.uo_selcliente=create uo_selcliente
this.uo_selplanta=create uo_selplanta
this.uo_SelPacking=create uo_SelPacking
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.st_1
this.Control[iCurrent+2]=this.st_2
this.Control[iCurrent+3]=this.em_desde
this.Control[iCurrent+4]=this.st_6
this.Control[iCurrent+5]=this.st_3
this.Control[iCurrent+6]=this.st_7
this.Control[iCurrent+7]=this.em_hasta
this.Control[iCurrent+8]=this.st_variedad
this.Control[iCurrent+9]=this.st_5
this.Control[iCurrent+10]=this.st_4
this.Control[iCurrent+11]=this.uo_selespecie
this.Control[iCurrent+12]=this.uo_selvariedad
this.Control[iCurrent+13]=this.st_8
this.Control[iCurrent+14]=this.uo_selcliente
this.Control[iCurrent+15]=this.uo_selplanta
this.Control[iCurrent+16]=this.uo_SelPacking
end on

on w_info_planificallegadadesdepacking.destroy
call super::destroy
destroy(this.st_1)
destroy(this.st_2)
destroy(this.em_desde)
destroy(this.st_6)
destroy(this.st_3)
destroy(this.st_7)
destroy(this.em_hasta)
destroy(this.st_variedad)
destroy(this.st_5)
destroy(this.st_4)
destroy(this.uo_selespecie)
destroy(this.uo_selvariedad)
destroy(this.st_8)
destroy(this.uo_selcliente)
destroy(this.uo_selplanta)
destroy(this.uo_SelPacking)
end on

event open;call super::open;Boolean lb_Cerrar

If IsNull(uo_SelCliente.Codigo) THEN lb_Cerrar = True
If IsNull(uo_SelPlanta.Codigo) THEN lb_Cerrar = True
If IsNull(uo_SelEspecie.Codigo) THEN lb_Cerrar = True
IF IsNull(uo_SelVariedad.Codigo) THEN lb_Cerrar = True
IF IsNull(uo_SelPacking.Codigo) THEN lb_Cerrar = True

IF lb_Cerrar THEN
	Close(This)
ELSE
	uo_SelCliente.Seleccion(False,False)
	uo_SelPlanta.Seleccion(False,False)
	uo_SelPacking.Seleccion(True,False)
	uo_SelEspecie.Seleccion(True,False)
	uo_SelVariedad.Seleccion(True,False)
	
	uo_SelPlanta.Filtra(1)
	uo_SelPacking.Filtra(2)
	uo_SelCliente.Inicia(gi_CodExport)
	uo_SelPlanta.Inicia(gi_CodPlanta)
	
	em_desde.Text				=	String(RelativeDate(today(), -365)) +' '+String(now())
	em_hasta.Text				=	String(Today())+' '+String(now())
End If



end event

event resize;//
end event

type pb_excel from w_para_informes`pb_excel within w_info_planificallegadadesdepacking
end type

type st_computador from w_para_informes`st_computador within w_info_planificallegadadesdepacking
end type

type st_usuario from w_para_informes`st_usuario within w_info_planificallegadadesdepacking
end type

type st_temporada from w_para_informes`st_temporada within w_info_planificallegadadesdepacking
end type

type p_logo from w_para_informes`p_logo within w_info_planificallegadadesdepacking
end type

type st_titulo from w_para_informes`st_titulo within w_info_planificallegadadesdepacking
integer width = 1705
string text = "Planificación llegada desde packing"
end type

type pb_acepta from w_para_informes`pb_acepta within w_info_planificallegadadesdepacking
integer x = 2112
integer y = 888
integer taborder = 130
end type

event pb_acepta::clicked;SetPointer(Arrow!)

Integer	fila

istr_info.titulo	= 'PLANIFICACION DE LLEGADA DESDE PACKING'	

OpenWithParm(vinf, istr_info)
vinf.dw_1.DataObject = "dw_infoplanificacionllegadapacking"
vinf.dw_1.SetTransObject(sqlca)

fila	=	vinf.dw_1.Retrieve(uo_SelCliente.Codigo, uo_SelPlanta.Codigo, uo_SelEspecie.Codigo, &
				uo_SelVariedad.Codigo, DateTime(em_Desde.Text), DateTime(em_Hasta.Text), uo_SelPacking.Codigo)

IF fila = -1 THEN
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
ELSEIF fila = 0 THEN
	MessageBox( "No Existe información", "No existe información para este informe.", StopSign!, Ok!)
ELSE
	F_Membrete(vinf.dw_1)
	vinf.dw_1.Modify("fechas.text = '" + "test fecha" + "'")	
	IF gs_Ambiente <> 'Windows' THEN F_ImprimeInformePdf(vinf.dw_1, istr_info.titulo)
END IF

SetPointer(Arrow!)				
end event

type pb_salir from w_para_informes`pb_salir within w_info_planificallegadadesdepacking
integer x = 2117
integer y = 1156
integer taborder = 140
end type

type st_1 from statictext within w_info_planificallegadadesdepacking
integer x = 293
integer y = 636
integer width = 352
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
boolean enabled = false
string text = "Planta"
boolean focusrectangle = false
end type

type st_2 from statictext within w_info_planificallegadadesdepacking
integer x = 293
integer y = 1380
integer width = 229
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
string text = "Desde"
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type em_desde from editmask within w_info_planificallegadadesdepacking
integer x = 521
integer y = 1364
integer width = 585
integer height = 96
integer taborder = 70
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 33554432
long backcolor = 16777215
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = datetimemask!
string mask = "dd/mm/yyyy hh:mm"
end type

type st_6 from statictext within w_info_planificallegadadesdepacking
integer x = 293
integer y = 496
integer width = 352
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

type st_3 from statictext within w_info_planificallegadadesdepacking
integer x = 293
integer y = 984
integer width = 352
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
string text = "Especie"
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type st_7 from statictext within w_info_planificallegadadesdepacking
integer x = 1125
integer y = 1380
integer width = 187
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
string text = "Hasta"
boolean focusrectangle = false
end type

type em_hasta from editmask within w_info_planificallegadadesdepacking
integer x = 1339
integer y = 1364
integer width = 585
integer height = 96
integer taborder = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 33554432
long backcolor = 16777215
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = datetimemask!
string mask = "dd/mm/yyyy hh:mm"
end type

type st_variedad from statictext within w_info_planificallegadadesdepacking
integer x = 293
integer y = 1168
integer width = 352
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
string text = "Variedad"
boolean focusrectangle = false
end type

type st_5 from statictext within w_info_planificallegadadesdepacking
integer x = 293
integer y = 824
integer width = 352
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
string text = "Packing"
boolean focusrectangle = false
end type

type st_4 from statictext within w_info_planificallegadadesdepacking
integer x = 251
integer y = 440
integer width = 1705
integer height = 868
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

type uo_selespecie from uo_seleccion_especie within w_info_planificallegadadesdepacking
event destroy ( )
integer x = 649
integer y = 900
integer height = 180
integer taborder = 80
boolean bringtotop = true
end type

on uo_selespecie.destroy
call uo_seleccion_especie::destroy
end on

event ue_cambio;call super::ue_cambio;IF IsNull(This.Codigo) THEN RETURN

CHOOSE CASE This.Codigo
	CASE -1, -9
		uo_selvariedad.cbx_Todos.Checked			=	True
		uo_selvariedad.cbx_Consolida.Enabled	=	True
		uo_selvariedad.dw_Seleccion.Enabled		=	False
		
	CASE ELSE
		uo_selvariedad.Filtra(This.Codigo)
		uo_selvariedad.dw_Seleccion.Enabled		=	True
END CHOOSE
end event

type uo_selvariedad from uo_seleccion_variedad within w_info_planificallegadadesdepacking
event destroy ( )
integer x = 649
integer y = 1088
integer height = 200
integer taborder = 90
boolean bringtotop = true
end type

on uo_selvariedad.destroy
call uo_seleccion_variedad::destroy
end on

type st_8 from statictext within w_info_planificallegadadesdepacking
integer x = 251
integer y = 1308
integer width = 1705
integer height = 184
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

type uo_selcliente from uo_seleccion_clientesprod within w_info_planificallegadadesdepacking
event destroy ( )
integer x = 649
integer y = 480
integer height = 100
integer taborder = 20
boolean bringtotop = true
end type

on uo_selcliente.destroy
call uo_seleccion_clientesprod::destroy
end on

type uo_selplanta from uo_seleccion_plantas within w_info_planificallegadadesdepacking
event destroy ( )
integer x = 649
integer y = 624
integer height = 100
integer taborder = 30
boolean bringtotop = true
end type

on uo_selplanta.destroy
call uo_seleccion_plantas::destroy
end on

type uo_SelPacking from uo_seleccion_plantas within w_info_planificallegadadesdepacking
event destroy ( )
integer x = 649
integer y = 724
integer taborder = 20
boolean bringtotop = true
end type

on uo_SelPacking.destroy
call uo_seleccion_plantas::destroy
end on


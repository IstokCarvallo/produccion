$PBExportHeader$w_info_historiapalletprefiro.srw
$PBExportComments$Informe Consolidado General de Embarques.
forward
global type w_info_historiapalletprefiro from w_para_informes
end type
type st_4 from statictext within w_info_historiapalletprefiro
end type
type st_3 from statictext within w_info_historiapalletprefiro
end type
type st_1 from statictext within w_info_historiapalletprefiro
end type
type em_desde from editmask within w_info_historiapalletprefiro
end type
type st_2 from statictext within w_info_historiapalletprefiro
end type
type uo_selcliente from uo_seleccion_clientesprod within w_info_historiapalletprefiro
end type
type uo_selplanta from uo_seleccion_plantas within w_info_historiapalletprefiro
end type
type st_6 from statictext within w_info_historiapalletprefiro
end type
type uo_selpacking from uo_seleccion_packing within w_info_historiapalletprefiro
end type
type st_5 from statictext within w_info_historiapalletprefiro
end type
type st_8 from statictext within w_info_historiapalletprefiro
end type
type em_numero from editmask within w_info_historiapalletprefiro
end type
type cbx_numero from checkbox within w_info_historiapalletprefiro
end type
type uo_selespecie from uo_seleccion_especie within w_info_historiapalletprefiro
end type
type cbx_fecha from checkbox within w_info_historiapalletprefiro
end type
type uo_selvariedad from uo_seleccion_variedad within w_info_historiapalletprefiro
end type
type st_7 from statictext within w_info_historiapalletprefiro
end type
type st_9 from statictext within w_info_historiapalletprefiro
end type
type uo_selembalajes from uo_seleccion_embalaje1 within w_info_historiapalletprefiro
end type
type st_10 from statictext within w_info_historiapalletprefiro
end type
type st_11 from statictext within w_info_historiapalletprefiro
end type
type em_recepcion from editmask within w_info_historiapalletprefiro
end type
type cbx_recepcion from checkbox within w_info_historiapalletprefiro
end type
end forward

global type w_info_historiapalletprefiro from w_para_informes
integer width = 3643
integer height = 1264
string title = "Historico Pallet en Prefrio"
event ue_listo ( )
st_4 st_4
st_3 st_3
st_1 st_1
em_desde em_desde
st_2 st_2
uo_selcliente uo_selcliente
uo_selplanta uo_selplanta
st_6 st_6
uo_selpacking uo_selpacking
st_5 st_5
st_8 st_8
em_numero em_numero
cbx_numero cbx_numero
uo_selespecie uo_selespecie
cbx_fecha cbx_fecha
uo_selvariedad uo_selvariedad
st_7 st_7
st_9 st_9
uo_selembalajes uo_selembalajes
st_10 st_10
st_11 st_11
em_recepcion em_recepcion
cbx_recepcion cbx_recepcion
end type
global w_info_historiapalletprefiro w_info_historiapalletprefiro

type variables
str_mant istr_mant

Long		ii_productor, ii_Tipo

Date		id_FechaZarpe, id_FechaAcceso
Time		it_HoraAcceso


end variables

forward prototypes
public function boolean existeproductor (long productor)
public function string buscaregionplanta (integer planta)
public function integer buscaplantasag (integer planta)
public function boolean existeplanilla (long al_planilla)
end prototypes

event ue_listo();w_main.SetMicroHelp("Listo")
SetPointer(Arrow!)

end event

public function boolean existeproductor (long productor);String	ls_Nombre

SELECT	prod_nombre
	INTO	:ls_Nombre
	FROM	dbo.productores
	WHERE	prod_codigo	=	:Productor ;
	
IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura de tabla Productor")
	
	RETURN False
ELSEIF sqlca.SQLCode = 100 THEN
	MessageBox("Atención", "Código de Productor no ha sido Definido.~r~r" + &
					"Ingrese o seleccione otro Código.")
	RETURN False
ELSE
	ii_productor = Productor
	RETURN True
END IF
end function

public function string buscaregionplanta (integer planta);Integer	li_region
String	ls_region

ls_region	=	'0'

SELECT plde_region
INTO	:li_region
FROM dbo.PLANTADESP
WHERE	plde_codigo=:Planta;

IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura de tabla Plantadesp")
	
	RETURN ls_region
	
ELSEIF sqlca.SQLCode = 100 THEN
	RETURN ls_region
END IF	

ls_region	=	String(li_region)

IF li_region = 13 THEN
	RETURN "M"
ELSE
	RETURN ls_region
END IF

end function

public function integer buscaplantasag (integer planta);Long	li_codmul

SELECT plde_codmul
INTO	:li_codmul
FROM dbo.PLANTADESP
WHERE	plde_codigo=:Planta;

IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura de tabla Plantadesp")
	
	RETURN li_codmul
	
ELSEIF sqlca.SQLCode = 100 THEN
	RETURN li_codmul
END IF	

RETURN li_codmul

end function

public function boolean existeplanilla (long al_planilla);Integer	li_codexp, li_planta
Date		ld_fecha

li_codexp		=	Integer(istr_mant.argumento[1])
li_planta		=	Integer(istr_mant.argumento[2])

IF (al_planilla <> 0) OR li_planta = 0 THEN
	
	SELECT Min(defe_fecdes)
		INTO	:ld_fecha
		FROM	dbo.DESPAFRIGOEN 
		WHERE	plde_codigo =	:li_planta
		AND	clie_codigo	=	:li_codexp
		AND	defe_plasag	=	:al_planilla ;
				
	IF sqlca.SQLCode = -1 THEN
		F_ErrorBaseDatos(sqlca,"Lectura de la Tabla DESPAFRIGOEN")

		RETURN False
	ELSEIF sqlca.SQLCode = 100 THEN
		MessageBox("Atención", "No existe Planilla S.A.G. Indicado.~r~rIngrese otro Número.", &
						Exclamation!, Ok!)
		pb_acepta.Enabled	= False
		RETURN False
	ELSEIF IsNull(ld_fecha) THEN
					MessageBox("Atención", "No existe Planilla S.A.G. Indicado.~r~rIngrese otro Número.", &
									Exclamation!, Ok!)
					pb_acepta.Enabled	= False
					RETURN False
		 ELSE
					pb_acepta.Enabled	= True
					RETURN True
		 END IF
ELSE
	MessageBox("Atención", "Faltan parámetros de búsqueda.~r~rIngreselos todos.", &
					Exclamation!, Ok!)
	RETURN False
END IF
end function

on w_info_historiapalletprefiro.create
int iCurrent
call super::create
this.st_4=create st_4
this.st_3=create st_3
this.st_1=create st_1
this.em_desde=create em_desde
this.st_2=create st_2
this.uo_selcliente=create uo_selcliente
this.uo_selplanta=create uo_selplanta
this.st_6=create st_6
this.uo_selpacking=create uo_selpacking
this.st_5=create st_5
this.st_8=create st_8
this.em_numero=create em_numero
this.cbx_numero=create cbx_numero
this.uo_selespecie=create uo_selespecie
this.cbx_fecha=create cbx_fecha
this.uo_selvariedad=create uo_selvariedad
this.st_7=create st_7
this.st_9=create st_9
this.uo_selembalajes=create uo_selembalajes
this.st_10=create st_10
this.st_11=create st_11
this.em_recepcion=create em_recepcion
this.cbx_recepcion=create cbx_recepcion
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.st_4
this.Control[iCurrent+2]=this.st_3
this.Control[iCurrent+3]=this.st_1
this.Control[iCurrent+4]=this.em_desde
this.Control[iCurrent+5]=this.st_2
this.Control[iCurrent+6]=this.uo_selcliente
this.Control[iCurrent+7]=this.uo_selplanta
this.Control[iCurrent+8]=this.st_6
this.Control[iCurrent+9]=this.uo_selpacking
this.Control[iCurrent+10]=this.st_5
this.Control[iCurrent+11]=this.st_8
this.Control[iCurrent+12]=this.em_numero
this.Control[iCurrent+13]=this.cbx_numero
this.Control[iCurrent+14]=this.uo_selespecie
this.Control[iCurrent+15]=this.cbx_fecha
this.Control[iCurrent+16]=this.uo_selvariedad
this.Control[iCurrent+17]=this.st_7
this.Control[iCurrent+18]=this.st_9
this.Control[iCurrent+19]=this.uo_selembalajes
this.Control[iCurrent+20]=this.st_10
this.Control[iCurrent+21]=this.st_11
this.Control[iCurrent+22]=this.em_recepcion
this.Control[iCurrent+23]=this.cbx_recepcion
end on

on w_info_historiapalletprefiro.destroy
call super::destroy
destroy(this.st_4)
destroy(this.st_3)
destroy(this.st_1)
destroy(this.em_desde)
destroy(this.st_2)
destroy(this.uo_selcliente)
destroy(this.uo_selplanta)
destroy(this.st_6)
destroy(this.uo_selpacking)
destroy(this.st_5)
destroy(this.st_8)
destroy(this.em_numero)
destroy(this.cbx_numero)
destroy(this.uo_selespecie)
destroy(this.cbx_fecha)
destroy(this.uo_selvariedad)
destroy(this.st_7)
destroy(this.st_9)
destroy(this.uo_selembalajes)
destroy(this.st_10)
destroy(this.st_11)
destroy(this.em_recepcion)
destroy(this.cbx_recepcion)
end on

event open;call super::open;Boolean	lb_Cerrar

If IsNull(uo_SelCliente.Codigo) Then lb_Cerrar = True
If IsNull(uo_SelPlanta.Codigo) Then lb_Cerrar = True
If IsNull(uo_SelPacking.Codigo) Then lb_Cerrar = True
If IsNull(uo_SelEspecie.Codigo) Then lb_Cerrar = True
If IsNull(uo_SelVariedad.Codigo) Then lb_Cerrar = True
If IsNull(uo_SelEmbalajes.Codigo) Then lb_Cerrar = True

If lb_Cerrar Then
	Close(This)
Else
	uo_SelCliente.Seleccion(False, False)
	uo_SelPlanta.Seleccion(True, False)
	uo_SelPacking.Seleccion(True, False)
	uo_SelEspecie.Seleccion(True, False)
	uo_SelVariedad.Seleccion(True, False)
	uo_SelEmbalajes.Seleccion(True, False)
	
	uo_SelPlanta.Filtra(1)
	
	uo_SelVariedad.Filtra(-1)
	uo_SelEmbalajes.Filtra(-1, '*')

	uo_SelCliente.Inicia(gi_codexport)	
	uo_SelPlanta.Inicia(gi_codplanta)
	
	em_desde.Text				=	String(Today())
	
	GrabaAccesoAplicacion(True, id_FechaAcceso, it_HoraAcceso, This.Title, "Acceso a Aplicación", 1)
End If
end event

event close;call super::close;GrabaAccesoAplicacion(False, id_FechaAcceso, it_HoraAcceso, "", "", 0)
end event

type pb_excel from w_para_informes`pb_excel within w_info_historiapalletprefiro
integer x = 3205
integer y = 292
end type

type st_computador from w_para_informes`st_computador within w_info_historiapalletprefiro
integer x = 2894
integer y = 164
end type

type st_usuario from w_para_informes`st_usuario within w_info_historiapalletprefiro
integer x = 2894
integer y = 96
end type

type st_temporada from w_para_informes`st_temporada within w_info_historiapalletprefiro
integer x = 2894
integer y = 20
end type

type p_logo from w_para_informes`p_logo within w_info_historiapalletprefiro
string picturename = "\Desarrollo 17\Imagenes\Logos\RBlanco.jpg"
end type

type st_titulo from w_para_informes`st_titulo within w_info_historiapalletprefiro
integer width = 2775
string text = "Historico Pallet en Prefrio"
end type

type pb_acepta from w_para_informes`pb_acepta within w_info_historiapalletprefiro
string tag = "Imprimir Reporte"
integer x = 3214
integer y = 464
integer taborder = 110
end type

event pb_acepta::clicked;SetPointer(Arrow!)

Long	ll_Fila, ll_Numero = -1
Date	ld_Desde, ld_DesdeR

istr_info.titulo	= 'INFORME HITORICO PALLET EN PREFRIO'
OpenWithParm(vinf, istr_info)
vinf.dw_1.DataObject = "dw_info_historicopalletpf"
vinf.dw_1.SetTransObject(sqlca)

If cbx_Recepcion.Checked Then
	ld_DesdeR	=	Date(Date('19000101'))
Else
	ld_DesdeR	=	Date(em_Recepcion.Text)
End If

If cbx_Fecha.Checked Then
	ld_Desde =	Date(Date('19000101'))
Else
	ld_Desde	=	Date(em_Desde.Text)
End If

If Not cbx_numero.Checked Then ll_Numero = Long(em_Numero.Text)

ll_fila	=	vinf.dw_1.Retrieve(uo_SelCliente.Codigo, uo_SelPlanta.Codigo, uo_SelPacking.Codigo, ll_Numero, &
				ld_Desde, uo_SelEspecie.Codigo, uo_SelVariedad.Codigo, uo_SelEmbalajes.Codigo, ld_DesdeR)

If ll_fila	 = -1 Then
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
ElseIf ll_fila	= 0 Then
	MessageBox( "No Existe información", "No existe información para este informe.", StopSign!, Ok!)
Else
	F_Membrete(vinf.dw_1)
	vinf.dw_1.Object.Datawindow.Zoom = 55
	If gs_Ambiente <> 'Windows' Then F_ImprimeInformePdf(vinf.dw_1, istr_info.titulo)
End If

SetPointer(Arrow!)
end event

type pb_salir from w_para_informes`pb_salir within w_info_historiapalletprefiro
string tag = "Salir [Cerrar Ventana Activa]"
integer x = 3214
integer y = 752
integer taborder = 120
string powertiptext = "Salir [Cerrar Ventana Activa]"
end type

type st_4 from statictext within w_info_historiapalletprefiro
integer x = 1655
integer y = 1616
integer width = 279
integer height = 64
integer taborder = 40
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 33543637
string text = "Variedad"
boolean focusrectangle = false
end type

type st_3 from statictext within w_info_historiapalletprefiro
integer x = 343
integer y = 552
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

type st_1 from statictext within w_info_historiapalletprefiro
integer x = 247
integer y = 408
integer width = 1385
integer height = 704
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

type em_desde from editmask within w_info_historiapalletprefiro
integer x = 2126
integer y = 940
integer width = 517
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
boolean enabled = false
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = datemask!
string mask = "dd/mm/yyyy"
boolean dropdowncalendar = true
end type

type st_2 from statictext within w_info_historiapalletprefiro
integer x = 1719
integer y = 956
integer width = 361
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
string text = "F. Embalaje"
boolean focusrectangle = false
end type

type uo_selcliente from uo_seleccion_clientesprod within w_info_historiapalletprefiro
event destroy ( )
integer x = 663
integer y = 540
integer height = 88
integer taborder = 50
boolean bringtotop = true
end type

on uo_selcliente.destroy
call uo_seleccion_clientesprod::destroy
end on

event ue_cambio;call super::ue_cambio;If IsNull(This.Codigo) Then Return

Choose Case This.Codigo
	Case -1, -9
		uo_SelEmbalajes.Filtra(-1, '*')
		
	Case Else
		uo_SelEmbalajes.Filtra(This.Codigo, Mid(uo_SelEspecie.Nombre, 1, 1))
		
End Choose
end event

type uo_selplanta from uo_seleccion_plantas within w_info_historiapalletprefiro
event destroy ( )
integer x = 663
integer y = 684
integer taborder = 50
boolean bringtotop = true
end type

on uo_selplanta.destroy
call uo_seleccion_plantas::destroy
end on

type st_6 from statictext within w_info_historiapalletprefiro
integer x = 1710
integer y = 572
integer width = 238
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
string text = "Especie"
boolean focusrectangle = false
end type

type uo_selpacking from uo_seleccion_packing within w_info_historiapalletprefiro
integer x = 613
integer y = 1540
integer taborder = 130
boolean bringtotop = true
end type

on uo_selpacking.destroy
call uo_seleccion_packing::destroy
end on

type st_5 from statictext within w_info_historiapalletprefiro
integer x = 1641
integer y = 408
integer width = 1385
integer height = 704
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

type st_8 from statictext within w_info_historiapalletprefiro
integer x = 343
integer y = 952
integer width = 288
integer height = 76
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 16777215
long backcolor = 553648127
boolean enabled = false
string text = "Nro. Guia"
boolean focusrectangle = false
end type

type em_numero from editmask within w_info_historiapalletprefiro
integer x = 663
integer y = 932
integer width = 494
integer height = 112
integer taborder = 70
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean enabled = false
borderstyle borderstyle = stylelowered!
string mask = "########"
end type

type cbx_numero from checkbox within w_info_historiapalletprefiro
integer x = 1253
integer y = 948
integer width = 306
integer height = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 16777215
long backcolor = 553648127
string text = "Todos"
boolean checked = true
end type

event clicked;If This.Checked Then
	em_numero.Enabled = False
	em_numero.Text = ''
Else
	em_numero.Enabled = True
	em_numero.Text = ''
End If
end event

type uo_selespecie from uo_seleccion_especie within w_info_historiapalletprefiro
integer x = 1998
integer y = 480
integer taborder = 60
boolean bringtotop = true
end type

on uo_selespecie.destroy
call uo_seleccion_especie::destroy
end on

event ue_cambio;call super::ue_cambio;If IsNull(This.Codigo) Then Return

Choose Case This.Codigo
	Case -1, -9
		uo_SelVariedad.Filtra(0)
		uo_SelEmbalajes.Filtra(uo_SelCliente.Codigo, '*')
		
	Case Else
		uo_SelVariedad.Filtra(This.Codigo)
		uo_SelEmbalajes.Filtra(uo_SelCliente.Codigo, Mid(This.Nombre, 1, 1))
		
End Choose
end event

type cbx_fecha from checkbox within w_info_historiapalletprefiro
integer x = 2656
integer y = 948
integer width = 256
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
string text = "Todos"
boolean checked = true
end type

event clicked;If This.Checked Then
	em_Desde.Enabled = False
	em_Desde.Text = ''
Else
	em_Desde.Enabled = True
	em_Desde.Text	= String(Today(), 'dd/mm/yyyy')
End If
end event

type uo_selvariedad from uo_seleccion_variedad within w_info_historiapalletprefiro
integer x = 1938
integer y = 1528
integer taborder = 140
boolean bringtotop = true
end type

on uo_selvariedad.destroy
call uo_seleccion_variedad::destroy
end on

type st_7 from statictext within w_info_historiapalletprefiro
integer x = 343
integer y = 788
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

type st_9 from statictext within w_info_historiapalletprefiro
integer x = 352
integer y = 1628
integer width = 247
integer height = 64
integer taborder = 40
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 33543637
string text = "Packing"
boolean focusrectangle = false
end type

type uo_selembalajes from uo_seleccion_embalaje1 within w_info_historiapalletprefiro
integer x = 1938
integer y = 1752
integer taborder = 50
boolean bringtotop = true
end type

on uo_selembalajes.destroy
call uo_seleccion_embalaje1::destroy
end on

type st_10 from statictext within w_info_historiapalletprefiro
integer x = 1655
integer y = 1844
integer width = 288
integer height = 64
integer taborder = 40
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 33543637
string text = "Embalaje"
boolean focusrectangle = false
end type

type st_11 from statictext within w_info_historiapalletprefiro
integer x = 1719
integer y = 788
integer width = 379
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
string text = "F. Recepcion"
boolean focusrectangle = false
end type

type em_recepcion from editmask within w_info_historiapalletprefiro
integer x = 2126
integer y = 772
integer width = 517
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
boolean enabled = false
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = datemask!
string mask = "dd/mm/yyyy"
boolean dropdowncalendar = true
end type

type cbx_recepcion from checkbox within w_info_historiapalletprefiro
integer x = 2656
integer y = 780
integer width = 256
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
string text = "Todos"
boolean checked = true
end type

event clicked;If This.Checked Then
	em_recepcion.Enabled = False
	em_recepcion.Text = ''
Else
	em_recepcion.Enabled = True
	em_recepcion.Text	= String(Today(), 'dd/mm/yyyy')
End If
end event


$PBExportHeader$w_info_balancemaza.srw
forward
global type w_info_balancemaza from w_para_informes
end type
type st_3 from statictext within w_info_balancemaza
end type
type st_1 from statictext within w_info_balancemaza
end type
type uo_selcliente from uo_seleccion_clientesprod within w_info_balancemaza
end type
type uo_selplanta from uo_seleccion_plantas within w_info_balancemaza
end type
type st_6 from statictext within w_info_balancemaza
end type
type cbx_resumen from checkbox within w_info_balancemaza
end type
type uo_selespecie from uo_seleccion_especie within w_info_balancemaza
end type
type st_7 from statictext within w_info_balancemaza
end type
type rb_gral from radiobutton within w_info_balancemaza
end type
type rb_es from radiobutton within w_info_balancemaza
end type
type rb_proceso from radiobutton within w_info_balancemaza
end type
end forward

global type w_info_balancemaza from w_para_informes
integer width = 3173
integer height = 1584
event ue_listo ( )
st_3 st_3
st_1 st_1
uo_selcliente uo_selcliente
uo_selplanta uo_selplanta
st_6 st_6
cbx_resumen cbx_resumen
uo_selespecie uo_selespecie
st_7 st_7
rb_gral rb_gral
rb_es rb_es
rb_proceso rb_proceso
end type
global w_info_balancemaza w_info_balancemaza

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

on w_info_balancemaza.create
int iCurrent
call super::create
this.st_3=create st_3
this.st_1=create st_1
this.uo_selcliente=create uo_selcliente
this.uo_selplanta=create uo_selplanta
this.st_6=create st_6
this.cbx_resumen=create cbx_resumen
this.uo_selespecie=create uo_selespecie
this.st_7=create st_7
this.rb_gral=create rb_gral
this.rb_es=create rb_es
this.rb_proceso=create rb_proceso
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.st_3
this.Control[iCurrent+2]=this.st_1
this.Control[iCurrent+3]=this.uo_selcliente
this.Control[iCurrent+4]=this.uo_selplanta
this.Control[iCurrent+5]=this.st_6
this.Control[iCurrent+6]=this.cbx_resumen
this.Control[iCurrent+7]=this.uo_selespecie
this.Control[iCurrent+8]=this.st_7
this.Control[iCurrent+9]=this.rb_gral
this.Control[iCurrent+10]=this.rb_es
this.Control[iCurrent+11]=this.rb_proceso
end on

on w_info_balancemaza.destroy
call super::destroy
destroy(this.st_3)
destroy(this.st_1)
destroy(this.uo_selcliente)
destroy(this.uo_selplanta)
destroy(this.st_6)
destroy(this.cbx_resumen)
destroy(this.uo_selespecie)
destroy(this.st_7)
destroy(this.rb_gral)
destroy(this.rb_es)
destroy(this.rb_proceso)
end on

event open;call super::open;Boolean	lb_Cerrar

If IsNull(uo_SelCliente.Codigo) Then lb_Cerrar = True
If IsNull(uo_SelPlanta.Codigo) Then lb_Cerrar = True
If IsNull(uo_SelEspecie.Codigo) Then lb_Cerrar = True

If lb_Cerrar Then
	Close(This)
Else
	uo_SelCliente.Seleccion(True, False)
	uo_SelPlanta.Seleccion(True, False)
	uo_SelEspecie.Seleccion(True, False)
	
	uo_SelPlanta.Filtra(1)
	
	GrabaAccesoAplicacion(True, id_FechaAcceso, it_HoraAcceso, This.Title, "Acceso a Aplicación", 1)
End If
end event

event close;call super::close;GrabaAccesoAplicacion(False, id_FechaAcceso, it_HoraAcceso, "", "", 0)
end event

type pb_excel from w_para_informes`pb_excel within w_info_balancemaza
integer x = 2711
integer y = 980
fontcharset fontcharset = ansi!
string facename = "Tahoma"
end type

type st_computador from w_para_informes`st_computador within w_info_balancemaza
integer x = 2894
integer y = 164
integer textsize = -10
fontfamily fontfamily = swiss!
string facename = "Tahoma"
end type

type st_usuario from w_para_informes`st_usuario within w_info_balancemaza
integer x = 2894
integer y = 96
integer textsize = -10
fontfamily fontfamily = swiss!
string facename = "Tahoma"
end type

type st_temporada from w_para_informes`st_temporada within w_info_balancemaza
integer x = 2894
integer y = 20
integer textsize = -10
fontfamily fontfamily = swiss!
string facename = "Tahoma"
end type

type p_logo from w_para_informes`p_logo within w_info_balancemaza
string picturename = "\Desarrollo 17\Imagenes\Logos\RBlanco.jpg"
end type

type st_titulo from w_para_informes`st_titulo within w_info_balancemaza
integer width = 2318
string text = "Balance de Masa"
end type

type pb_acepta from w_para_informes`pb_acepta within w_info_balancemaza
string tag = "Imprimir Reporte"
integer x = 2715
integer y = 448
integer taborder = 110
fontcharset fontcharset = ansi!
string facename = "Tahoma"
end type

event pb_acepta::clicked;SetPointer(Arrow!)

Long	ll_Fila, ll_resumen = 0
Date	ld_Desde

istr_info.titulo	= 'INFORME BALANCE DE MAZA'

OpenWithParm(vinf, istr_info)

If rb_gral.Checked Then 
	If cbx_resumen.Checked Then 
		ll_resumen = 1
		vinf.dw_1.DataObject = "dw_info_balancemaza_gral_res"
	Else
		vinf.dw_1.DataObject = "dw_info_balancemaza_gral"
	End If
ElseIf rb_es.Checked Then 
	vinf.dw_1.DataObject = "dw_info_balancemaza_movto"
Else
	vinf.dw_1.DataObject = "dw_info_balancemaza_proceso"
End If

vinf.dw_1.SetTransObject(sqlca)

ll_fila	=	vinf.dw_1.Retrieve(uo_SelCliente.Codigo, uo_SelPlanta.Codigo, uo_SelEspecie.Codigo, ll_resumen)

If ll_fila	 = -1 Then
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
ElseIf ll_fila	= 0 Then
	MessageBox( "No Existe información", "No existe información para este informe.", StopSign!, Ok!)
Else
	F_Membrete(vinf.dw_1)	
	If gs_Ambiente <> 'Windows' Then F_ImprimeInformePdf(vinf.dw_1, istr_info.titulo)
End If

SetPointer(Arrow!)
end event

type pb_salir from w_para_informes`pb_salir within w_info_balancemaza
string tag = "Salir [Cerrar Ventana Activa]"
integer x = 2715
integer y = 736
integer taborder = 120
fontcharset fontcharset = ansi!
string facename = "Tahoma"
string powertiptext = "Salir [Cerrar Ventana Activa]"
end type

type st_3 from statictext within w_info_balancemaza
integer x = 343
integer y = 680
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

type st_1 from statictext within w_info_balancemaza
integer x = 247
integer y = 408
integer width = 2318
integer height = 952
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 33554432
long backcolor = 16711680
boolean enabled = false
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type uo_selcliente from uo_seleccion_clientesprod within w_info_balancemaza
event destroy ( )
integer x = 603
integer y = 588
integer height = 180
integer taborder = 50
boolean bringtotop = true
end type

on uo_selcliente.destroy
call uo_seleccion_clientesprod::destroy
end on

type uo_selplanta from uo_seleccion_plantas within w_info_balancemaza
event destroy ( )
integer x = 603
integer y = 800
integer taborder = 50
boolean bringtotop = true
end type

on uo_selplanta.destroy
call uo_seleccion_plantas::destroy
end on

type st_6 from statictext within w_info_balancemaza
integer x = 343
integer y = 1100
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

type cbx_resumen from checkbox within w_info_balancemaza
integer x = 1650
integer y = 680
integer width = 681
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
string text = "Resumen Productor"
end type

type uo_selespecie from uo_seleccion_especie within w_info_balancemaza
integer x = 603
integer y = 1012
integer taborder = 60
boolean bringtotop = true
end type

on uo_selespecie.destroy
call uo_seleccion_especie::destroy
end on

type st_7 from statictext within w_info_balancemaza
integer x = 343
integer y = 896
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

type rb_gral from radiobutton within w_info_balancemaza
integer x = 603
integer y = 472
integer width = 457
integer height = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 16711680
string text = "General"
boolean checked = true
end type

event clicked;If This.Checked Then
	cbx_resumen.Checked = False
	cbx_resumen.Enabled = True
End If 
end event

type rb_es from radiobutton within w_info_balancemaza
integer x = 1202
integer y = 472
integer width = 485
integer height = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 16711680
string text = "Movimientos "
end type

event clicked;If This.Checked Then
	cbx_resumen.Checked = False
	cbx_resumen.Enabled = False
End If 
end event

type rb_proceso from radiobutton within w_info_balancemaza
integer x = 1874
integer y = 472
integer width = 457
integer height = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 16711680
string text = "Procesos"
end type

event clicked;If This.Checked Then
	cbx_resumen.Checked = False
	cbx_resumen.Enabled = False
End If 
end event


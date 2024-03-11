$PBExportHeader$w_proc_genera_facturacion.srw
$PBExportComments$Informe de Facturación Productor Mensual.
forward
global type w_proc_genera_facturacion from w_para_informes
end type
type st_4 from statictext within w_proc_genera_facturacion
end type
type st_1 from statictext within w_proc_genera_facturacion
end type
type st_2 from statictext within w_proc_genera_facturacion
end type
type em_fecha from editmask within w_proc_genera_facturacion
end type
type st_6 from statictext within w_proc_genera_facturacion
end type
type st_8 from statictext within w_proc_genera_facturacion
end type
type st_9 from statictext within w_proc_genera_facturacion
end type
type em_valorcambio from editmask within w_proc_genera_facturacion
end type
type em_iva from editmask within w_proc_genera_facturacion
end type
type st_3 from statictext within w_proc_genera_facturacion
end type
type rb_procesada from radiobutton within w_proc_genera_facturacion
end type
type rb_granel from radiobutton within w_proc_genera_facturacion
end type
type st_5 from statictext within w_proc_genera_facturacion
end type
type st_7 from statictext within w_proc_genera_facturacion
end type
type uo_selcliente from uo_seleccion_clientesprod within w_proc_genera_facturacion
end type
type uo_selplanta from uo_seleccion_plantas within w_proc_genera_facturacion
end type
type uo_selespecie from uo_seleccion_especie within w_proc_genera_facturacion
end type
type em_desde from editmask within w_proc_genera_facturacion
end type
type em_hasta from editmask within w_proc_genera_facturacion
end type
type st_10 from statictext within w_proc_genera_facturacion
end type
type st_11 from statictext within w_proc_genera_facturacion
end type
type em_numero from editmask within w_proc_genera_facturacion
end type
type st_12 from statictext within w_proc_genera_facturacion
end type
type dw_1 from uo_dw within w_proc_genera_facturacion
end type
type cb_valida from commandbutton within w_proc_genera_facturacion
end type
end forward

global type w_proc_genera_facturacion from w_para_informes
integer x = 14
integer y = 32
integer width = 2459
integer height = 2232
string title = "Facturación Mensual de Productores"
boolean minbox = false
boolean maxbox = false
boolean resizable = false
windowtype windowtype = response!
string icon = "F:\Desarrollo\Producción\FrutaProcesada\Producc.ico"
st_4 st_4
st_1 st_1
st_2 st_2
em_fecha em_fecha
st_6 st_6
st_8 st_8
st_9 st_9
em_valorcambio em_valorcambio
em_iva em_iva
st_3 st_3
rb_procesada rb_procesada
rb_granel rb_granel
st_5 st_5
st_7 st_7
uo_selcliente uo_selcliente
uo_selplanta uo_selplanta
uo_selespecie uo_selespecie
em_desde em_desde
em_hasta em_hasta
st_10 st_10
st_11 st_11
em_numero em_numero
st_12 st_12
dw_1 dw_1
cb_valida cb_valida
end type
global w_proc_genera_facturacion w_proc_genera_facturacion

type variables

end variables

forward prototypes
public function boolean existeproceso2 (integer cliente, integer planta, date fecha)
public function boolean wf_existeproceso (integer cliente, integer planta, date fecha, long numero)
public function long wf_obtienesecuencia (integer cliente, integer planta, date fecha)
public function boolean wf_existerango (integer cliente, integer planta, date periodo, date fecha)
end prototypes

public function boolean existeproceso2 (integer cliente, integer planta, date fecha);Long	ll_cuenta, ll_cuenta2


SELECT COUNT(*)
INTO  :ll_cuenta2
FROM dbo.facturprodenca_granel
WHERE clie_codigo=:Cliente
AND   plde_codigo=:Planta
AND   faen_fechaf=:Fecha;

IF sqlca.SQLCode = -1 THEN
	F_errorbasedatos(sqlca,"Lectura Tabla Facturprodenca_granel")
	RETURN True
ELSEIF ll_cuenta2 > 0 THEN
       RETURN True		 
ELSE
	    RETURN False
END IF

end function

public function boolean wf_existeproceso (integer cliente, integer planta, date fecha, long numero);Long	ll_cuenta

SELECT COUNT(*)
	INTO  :ll_cuenta
	FROM dbo.facturprodenca
	WHERE clie_codigo=:Cliente
		AND   plde_codigo=:Planta
		AND   faen_fechaf=:Fecha
		And	faen_secuen = :Numero
	USING SQLCA;

IF SQLCA.SQLCode = -1 THEN
	F_errorbasedatos(sqlca,"Lectura Tabla Facturprodenca")
	RETURN True
ELSEIF ll_cuenta > 0 THEN
       RETURN True		 
ELSE
	    RETURN False
END IF

end function

public function long wf_obtienesecuencia (integer cliente, integer planta, date fecha);Long	ll_Secuencia

SELECT IsNull(Max(faen_secuen), 0) + 1
	INTO  :ll_Secuencia
	FROM dbo.facturprodenca
	WHERE clie_codigo=:Cliente
		AND   faen_fechaf=:Fecha
	USING SQLCA;

If SQLCA.SQLCode = -1 Then
	F_errorbasedatos(sqlca,"Lectura Tabla Facturprodenca")
	Return -1
Else
	Return ll_Secuencia
End If

end function

public function boolean wf_existerango (integer cliente, integer planta, date periodo, date fecha);Boolean	lb_Retorno
Long		ll_Secuencia
String		ls_Fecha

ls_Fecha = String(Fecha, 'yyyymmdd')

SELECT Count(IsNull(faen_secuen, 0))
	INTO  :ll_Secuencia
	FROM dbo.facturprodenca
	WHERE clie_codigo=:Cliente
		AND plde_codigo=:Planta
		And faen_fechaf = :Periodo
		And :ls_Fecha between faen_fecini and faen_fecter
	USING SQLCA;

If SQLCA.SQLCode = -1 Then
	F_errorbasedatos(sqlca,"Lectura Tabla Facturprodenca")
	lb_Retorno = False
Else
	If ll_Secuencia > 0 Then
		lb_Retorno = True
	Else
		lb_Retorno = False
	End If
End If

Return lb_Retorno
end function

on w_proc_genera_facturacion.create
int iCurrent
call super::create
this.st_4=create st_4
this.st_1=create st_1
this.st_2=create st_2
this.em_fecha=create em_fecha
this.st_6=create st_6
this.st_8=create st_8
this.st_9=create st_9
this.em_valorcambio=create em_valorcambio
this.em_iva=create em_iva
this.st_3=create st_3
this.rb_procesada=create rb_procesada
this.rb_granel=create rb_granel
this.st_5=create st_5
this.st_7=create st_7
this.uo_selcliente=create uo_selcliente
this.uo_selplanta=create uo_selplanta
this.uo_selespecie=create uo_selespecie
this.em_desde=create em_desde
this.em_hasta=create em_hasta
this.st_10=create st_10
this.st_11=create st_11
this.em_numero=create em_numero
this.st_12=create st_12
this.dw_1=create dw_1
this.cb_valida=create cb_valida
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.st_4
this.Control[iCurrent+2]=this.st_1
this.Control[iCurrent+3]=this.st_2
this.Control[iCurrent+4]=this.em_fecha
this.Control[iCurrent+5]=this.st_6
this.Control[iCurrent+6]=this.st_8
this.Control[iCurrent+7]=this.st_9
this.Control[iCurrent+8]=this.em_valorcambio
this.Control[iCurrent+9]=this.em_iva
this.Control[iCurrent+10]=this.st_3
this.Control[iCurrent+11]=this.rb_procesada
this.Control[iCurrent+12]=this.rb_granel
this.Control[iCurrent+13]=this.st_5
this.Control[iCurrent+14]=this.st_7
this.Control[iCurrent+15]=this.uo_selcliente
this.Control[iCurrent+16]=this.uo_selplanta
this.Control[iCurrent+17]=this.uo_selespecie
this.Control[iCurrent+18]=this.em_desde
this.Control[iCurrent+19]=this.em_hasta
this.Control[iCurrent+20]=this.st_10
this.Control[iCurrent+21]=this.st_11
this.Control[iCurrent+22]=this.em_numero
this.Control[iCurrent+23]=this.st_12
this.Control[iCurrent+24]=this.dw_1
this.Control[iCurrent+25]=this.cb_valida
end on

on w_proc_genera_facturacion.destroy
call super::destroy
destroy(this.st_4)
destroy(this.st_1)
destroy(this.st_2)
destroy(this.em_fecha)
destroy(this.st_6)
destroy(this.st_8)
destroy(this.st_9)
destroy(this.em_valorcambio)
destroy(this.em_iva)
destroy(this.st_3)
destroy(this.rb_procesada)
destroy(this.rb_granel)
destroy(this.st_5)
destroy(this.st_7)
destroy(this.uo_selcliente)
destroy(this.uo_selplanta)
destroy(this.uo_selespecie)
destroy(this.em_desde)
destroy(this.em_hasta)
destroy(this.st_10)
destroy(this.st_11)
destroy(this.em_numero)
destroy(this.st_12)
destroy(this.dw_1)
destroy(this.cb_valida)
end on

event open;call super::open;Boolean	lb_Cerrar = False

If IsNull(uo_SelCliente.Codigo) Then lb_Cerrar = True
If IsNull(uo_SelPlanta.Codigo) Then lb_Cerrar = True
If IsNull(uo_SelEspecie.Codigo) Then lb_Cerrar = True

If lb_Cerrar Then
	Close(This)
Else
	dw_1.SetTransObject(Sqlca)
	
	uo_SelCliente.Seleccion(False, False)
	uo_SelPlanta.Seleccion(False, False)
	uo_SelEspecie.Seleccion(True, False)
	uo_SelCliente.Inicia(gi_CodExport)
	uo_SelPlanta.Inicia(gi_CodPlanta)
	
	em_Fecha.Text				=	String(Today())
	em_desde.Text				=	'01/' + em_Fecha.Text
	em_hasta.Text				=	String(Today())
	em_iva.Text					=	"19"
	
	dw_1.Retrieve(uo_SelCliente.Codigo, uo_SelPlanta.Codigo, Date('01/' + em_Fecha.Text), -1)
End If
end event

event resize;call super::resize;If cb_valida.Visible Then
	cb_valida.x			=	(pb_Acepta.x + (pb_Acepta.Width / 2)) - (cb_valida.Width / 2)
	cb_valida.y			=	pb_Acepta.y - 135
End If
end event

type pb_excel from w_para_informes`pb_excel within w_proc_genera_facturacion
integer taborder = 20
end type

type st_computador from w_para_informes`st_computador within w_proc_genera_facturacion
end type

type st_usuario from w_para_informes`st_usuario within w_proc_genera_facturacion
end type

type st_temporada from w_para_informes`st_temporada within w_proc_genera_facturacion
end type

type p_logo from w_para_informes`p_logo within w_proc_genera_facturacion
string picturename = "\Desarrollo 17\Imagenes\Logos\RBlanco.jpg"
end type

type st_titulo from w_para_informes`st_titulo within w_proc_genera_facturacion
integer width = 1687
string text = "Genera Información para Facturación de Productores"
end type

type pb_acepta from w_para_informes`pb_acepta within w_proc_genera_facturacion
integer x = 2053
integer y = 672
integer taborder = 90
integer weight = 400
fontcharset fontcharset = ansi!
boolean enabled = false
boolean default = false
string picturename = "\Desarrollo 17\Imagenes\Botones\Guardar Como.png"
string disabledname = "\Desarrollo 17\Imagenes\Botones\Guardar Como-bn.png"
end type

event pb_acepta::clicked;Integer		ll_Filas, li_Consolidado, li_respuesta, li_metodo, li_Numero
Date			ld_MesProceso
Decimal{2}	ld_PorIVA, ld_ValCambio
Boolean		lb_Existe
String			ls_desde, ls_hasta

SetPointer(HourGlass!)

ld_MesProceso 	= 	Date("01/" + em_Fecha.Text)
ls_desde			=	em_desde.Text
ls_hasta			=	em_hasta.Text
ld_PorIVA		=	Dec(em_iva.Text)
ld_ValCambio	=	Dec(em_ValorCambio.Text)
li_Numero		=	Integer(em_numero.Text)

If ld_MesProceso = Date('19000101') OR isnull(ld_MesProceso)  Then
	MessageBox('Atención!!','Falta Mes de Proceso para Continuar', Exclamation!, OK!, 2)
	em_fecha.SetFocus()
	Return
End If

If ld_ValCambio = 0 OR isnull(ld_ValCambio)  Then
	MessageBox('Atención!!','Falta Valor de cambio para Continuar', Exclamation!, OK!, 2)
	em_valorcambio.Setfocus()
	Return
End If	

If rb_procesada.Checked Then
	li_metodo 	=	1
	lb_Existe	=	wf_ExisteProceso(uo_SelCliente.Codigo,uo_SelPlanta.Codigo,ld_MesProceso, li_Numero)
End If

If lb_Existe Then
   li_respuesta = MessageBox('Atención!!','Proceso de Generación según parámetros ya se efectúo, desea re-procesar', Exclamation!, OKCancel!, 2)
   If li_respuesta = 1 Then
		If li_metodo =  1 Then					
			DECLARE Borra_Facturacion PROCEDURE FOR dbo.FProc_BorraGeneracionFacturacion
						@Cliente			=	:uo_SelCliente.Codigo,
						@Planta			=	:uo_SelPlanta.Codigo,
						@MesProceso	=	:ld_MesProceso,
						@Numero		=	:li_Numero;
						
			EXECUTE Borra_Facturacion;
			
			If SQLCA.SQLCode < 0 Then
				Commit; 
			Else
				Commit;
			End If
				
			li_Numero =	wf_ObtieneSecuencia(uo_SelCliente.Codigo,uo_SelPlanta.Codigo,ld_MesProceso)
			
			If li_Numero = -1 Then
				MessageBox('Error', 'No se pudo obtener sucuencia. informe a TI.', StopSign!, Ok!)
				Return
			End If
	
			DECLARE Re_Genera_Facturacion PROCEDURE FOR dbo.FProc_GeneracionFacturacionProductor
							@Cliente			=	:uo_SelCliente.Codigo,
							@Planta			=	:uo_SelPlanta.Codigo,
							@MesProceso	=	:ld_MesProceso,
							@Zona			=	-1,
							@Productor		=	-1,
							@Cambio		=	:ld_ValCambio,
							@PorIva			=	:ld_PorIva,
							@Especie       	=  :uo_SelEspecie.Codigo,
							@Numero 		=	:li_Numero,
							@Desde 			=	:ls_desde,
							@Hasta 			=	:ls_hasta;
			EXECUTE Re_Genera_Facturacion;
			If SQLCA.SQLCode < 0 Then
				MessageBox("Error en Genera Facturación", "Se ha producido un Error en Generación Facturación.~r~r" + &
								SQLCA.SQLErrText)
				Commit;
			Else
				MessageBox("Genera Facturación", "El Proceso Se Ha Ejecutado Correctamente.")
				Commit;
			End If
			
			em_numero.Text = ''
//		Else
//				DECLARE Re_Genera_GranelFacturacion PROCEDURE FOR dbo.FProc_GeneraGranelFacturacionProductor															
//							@Cliente			=	:uo_SelCliente.Codigo,
//							@Planta			=	:uo_SelPlanta.Codigo,
//							@MesProceso	=	:ld_MesProceso,
//							@Zona			=	-1,
//							@Productor		=	-1,
//							@Cambio		=	:ld_ValCambio,
//							@PorIva			=	:ld_PorIva,
//							@Especie       	=  :uo_SelEspecie.Codigo;
//				EXECUTE Re_Genera_GranelFacturacion;
//				If SQLCA.SQLCode < 0 Then
//					MessageBox("Error en Genera Granel Facturación", "Se ha producido un Error en Generación Granel Facturación.~r~r" + &
//									SQLCA.SQLErrText)
//					Commit;
//				Else
//					MessageBox("Genera Granel Facturación", "El Proceso Se Ha Ejecutado Correctamente.")
//					Commit;
//				End If	
		End If
   End If
	
Else
	
		li_Numero =	wf_ObtieneSecuencia(uo_SelCliente.Codigo,uo_SelPlanta.Codigo,ld_MesProceso)
		If li_Numero = -1 Then
			MessageBox('Error', 'No se pudo obtener sucuencia. informe a TI.', StopSign!, Ok!)
			Return
		End If
	
		If li_metodo =  1 Then					
			DECLARE Genera_Facturacion PROCEDURE FOR dbo.FProc_GeneracionFacturacionProductor
						@Cliente			=	:uo_SelCliente.Codigo,
						@Planta			=	:uo_SelPlanta.Codigo,
						@MesProceso	=	:ld_MesProceso,
						@Zona			=	-1,
						@Productor		=	-1,
						@Cambio		=	:ld_ValCambio,
						@PorIva			=	:ld_PorIva,
						@Especie       	=  :uo_SelEspecie.Codigo,
						@Numero 		=	:li_Numero,
						@Desde 			=	:ls_desde,
						@Hasta 			=	:ls_hasta;

			EXECUTE Genera_Facturacion;
			
			If SQLCA.SQLCode < 0 Then
				MessageBox("Error en Genera Facturación", "Se ha producido un Error en Generación Facturación.~r~r" + &
								SQLCA.SQLErrText)
				Commit;
			Else
				MessageBox("Genera Facturación", "El Proceso Se Ha Ejecutado Correctamente.")
				Commit;
			End If
		End If
End If

dw_1.Retrieve(uo_SelCliente.Codigo, uo_SelPlanta.Codigo, Date('01/' + em_Fecha.Text), -1)

SetPointer(Arrow!)
end event

type pb_salir from w_para_informes`pb_salir within w_proc_genera_facturacion
integer x = 2057
integer y = 992
integer taborder = 100
end type

type st_4 from statictext within w_proc_genera_facturacion
integer x = 247
integer y = 424
integer width = 1687
integer height = 704
boolean bringtotop = true
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

type st_1 from statictext within w_proc_genera_facturacion
integer x = 343
integer y = 612
integer width = 462
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
string text = "Planta"
boolean focusrectangle = false
end type

type st_2 from statictext within w_proc_genera_facturacion
integer x = 343
integer y = 740
integer width = 485
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
string text = "Mes de Proceso"
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type em_fecha from editmask within w_proc_genera_facturacion
integer x = 850
integer y = 724
integer width = 311
integer height = 96
integer taborder = 40
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 16777215
alignment alignment = center!
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = datemask!
string mask = "mm/yyyy"
end type

event modified;If IsNull(This.Text) Then Return

em_desde.Text		=	'01/' + This.Text
em_hasta.Text		=	String(RelativeDate(Date('01/' + Mid(String(RelativeDate(Date(em_desde.Text), 31)), 4, 7)), -1))
dw_1.Retrieve(uo_SelCliente.Codigo, uo_SelPlanta.Codigo, Date('01/' + This.Text), -1)

If wf_ExisteRango(uo_SelCliente.Codigo, uo_SelPlanta.Codigo, Date('01/' + This.Text), Date('01/' + This.Text)) Then
	MessageBox('Error', 'Fecha esta contenida dentro de un rango ya validado', StopSign!, Ok!)
	em_desde.Text = ''
	em_desde.SetFocus()
End If


end event

type st_6 from statictext within w_proc_genera_facturacion
integer x = 343
integer y = 492
integer width = 233
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

type st_8 from statictext within w_proc_genera_facturacion
integer x = 343
integer y = 988
integer width = 411
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
string text = "Valor Cambio"
boolean focusrectangle = false
end type

type st_9 from statictext within w_proc_genera_facturacion
integer x = 1285
integer y = 988
integer width = 279
integer height = 68
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 16777215
long backcolor = 553648127
string text = "%  I.V.A."
boolean focusrectangle = false
end type

type em_valorcambio from editmask within w_proc_genera_facturacion
integer x = 850
integer y = 972
integer width = 393
integer height = 96
integer taborder = 70
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 16777215
alignment alignment = right!
borderstyle borderstyle = stylelowered!
string mask = "##,###.##"
end type

type em_iva from editmask within w_proc_genera_facturacion
integer x = 1605
integer y = 972
integer width = 261
integer height = 96
integer taborder = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 16777215
alignment alignment = right!
borderstyle borderstyle = stylelowered!
string mask = "#0.00"
end type

type st_3 from statictext within w_proc_genera_facturacion
boolean visible = false
integer x = 430
integer y = 1896
integer width = 1687
integer height = 212
boolean bringtotop = true
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

type rb_procesada from radiobutton within w_proc_genera_facturacion
boolean visible = false
integer x = 544
integer y = 1960
integer width = 594
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
string text = "Fruta Procesada"
boolean checked = true
end type

type rb_granel from radiobutton within w_proc_genera_facturacion
boolean visible = false
integer x = 1298
integer y = 1960
integer width = 466
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
boolean enabled = false
string text = "Fruta Granel"
end type

type st_5 from statictext within w_proc_genera_facturacion
integer x = 247
integer y = 1128
integer width = 1687
integer height = 232
boolean bringtotop = true
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

type st_7 from statictext within w_proc_genera_facturacion
integer x = 357
integer y = 1252
integer width = 503
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
string text = "Especie"
boolean focusrectangle = false
end type

type uo_selcliente from uo_seleccion_clientesprod within w_proc_genera_facturacion
event destroy ( )
integer x = 850
integer y = 480
integer height = 92
integer taborder = 10
boolean bringtotop = true
end type

on uo_selcliente.destroy
call uo_seleccion_clientesprod::destroy
end on

type uo_selplanta from uo_seleccion_plantas within w_proc_genera_facturacion
event destroy ( )
integer x = 850
integer y = 604
integer height = 92
integer taborder = 30
boolean bringtotop = true
end type

on uo_selplanta.destroy
call uo_seleccion_plantas::destroy
end on

type uo_selespecie from uo_seleccion_especie within w_proc_genera_facturacion
event destroy ( )
integer x = 850
integer y = 1152
integer taborder = 110
boolean bringtotop = true
end type

on uo_selespecie.destroy
call uo_seleccion_especie::destroy
end on

type em_desde from editmask within w_proc_genera_facturacion
integer x = 850
integer y = 844
integer width = 379
integer height = 96
integer taborder = 50
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 16777215
alignment alignment = center!
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = datetimemask!
string mask = "dd/mm/yyyy"
end type

event modified;Date ld_desde, ld_Hasta, ld_Actual

If IsNull(This.Text) or This.Text = '00/00/0000' Then Return

If wf_ExisteRango(uo_SelCliente.Codigo, uo_SelPlanta.Codigo, Date("01/" + em_Fecha.Text), Date(This.Text)) Then
	MessageBox('Error', 'Fecha esta contenida dentro de un rango ya validado', StopSign!, Ok!)
	This.Text = ''
	em_Hasta.Text = ''
	THis.SetFocus()
Else
	ld_Desde	=	Date('01/' + em_Fecha.Text)
	ld_hasta	=	Date(String(RelativeDate(Date('01/' + Mid(String(RelativeDate(ld_Desde, 31)), 4, 7)), -1)))
	ld_Actual	=	Date(This.Text)
	
	If Not ((ld_Actual >= ld_Desde) And (ld_Actual <= ld_Hasta)) Then
		MessageBox('Error', 'Fecha no esta contenida dentro del periodo seleccionado.', StopSign!, Ok!)
		This.Text = ''
		em_Hasta.Text = ''
		THis.SetFocus()
	End If
End If
end event

type em_hasta from editmask within w_proc_genera_facturacion
integer x = 1486
integer y = 844
integer width = 379
integer height = 96
integer taborder = 60
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 16777215
alignment alignment = center!
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = datetimemask!
string mask = "dd/mm/yyyy"
end type

event modified;Date ld_desde, ld_Hasta, ld_Actual

If IsNull(This.Text) or This.Text = '00/00/0000' Then Return

If wf_ExisteRango(uo_SelCliente.Codigo, uo_SelPlanta.Codigo, Date("01/" + em_Fecha.Text), Date(This.Text)) Then
	MessageBox('Error', 'Fecha esta contenida dentro de un rango ya validado', StopSign!, Ok!)
	This.Text = ''
	em_Hasta.Text = ''
	THis.SetFocus()
Else
	ld_Desde	=	Date('01/' + em_Fecha.Text)
	ld_hasta	=	Date(String(RelativeDate(Date('01/' + Mid(String(RelativeDate(ld_Desde, 31)), 4, 7)), -1)))
	ld_Actual	=	Date(This.Text)
	
	If Not ((ld_Actual >= ld_Desde) And (ld_Actual <= ld_Hasta)) Then
		MessageBox('Error', 'Fecha no esta contenida dentro del periodo seleccionado.', StopSign!, Ok!)
		This.Text = ''
		em_Hasta.Text = ''
		THis.SetFocus()
	End If
End If
end event

type st_10 from statictext within w_proc_genera_facturacion
integer x = 343
integer y = 860
integer width = 485
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
string text = "Desde"
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type st_11 from statictext within w_proc_genera_facturacion
integer x = 1285
integer y = 860
integer width = 187
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
string text = "Hasta"
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type em_numero from editmask within w_proc_genera_facturacion
integer x = 1472
integer y = 724
integer width = 393
integer height = 96
integer taborder = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 16777215
long backcolor = 16711680
boolean enabled = false
alignment alignment = center!
borderstyle borderstyle = stylelowered!
string mask = "###"
end type

type st_12 from statictext within w_proc_genera_facturacion
integer x = 1285
integer y = 740
integer width = 187
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
string text = "Nro."
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type dw_1 from uo_dw within w_proc_genera_facturacion
integer x = 133
integer y = 1448
integer width = 1925
integer height = 668
integer taborder = 11
boolean bringtotop = true
string dataobject = "dw_mues_facturprodenca_resumen"
end type

event doubleclicked;call super::doubleclicked;If IsNull(Row) Or Row < 1 Then Return

If This.Object.faen_estado[Row] > 0 Then
	MessageBox('Error...', 'Proforma Seleccionada esta en estado distinto a generada, seleccione otra', StopSign!, OK!)
Else
	em_fecha.Text = String(This.Object.faen_fechaf[Row])
	em_desde.Text = String(This.Object.faen_fecini[Row], 'dd/mm/yyyy')
	em_hasta.Text = String(This.Object.faen_fecter[Row], 'dd/mm/yyyy')
	em_numero.Text = String(This.Object.faen_secuen[Row])
End If
end event

type cb_valida from commandbutton within w_proc_genera_facturacion
integer x = 2011
integer y = 480
integer width = 384
integer height = 116
integer taborder = 40
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "Validacion"
boolean default = true
boolean flatstyle = true
end type

event clicked;Long			ll_Fila
Date			ld_MesProceso
DateTime	ld_desde, ld_hasta

SetPointer(HourGlass!)

ld_MesProceso 	= 	Date("01/" + em_Fecha.Text)
 ld_desde		=	DateTime(em_desde.Text)
 ld_hasta			=	DateTime(em_hasta.Text)

istr_info.Titulo	=	"VALIDACION PROFORMA PRODUCTORES"
istr_info.copias	=	1

OpenWithParm(vinf, istr_info)
vinf.dw_1.DataObject = "dw_info_validaproforma"
vinf.dw_1.SetTransObject(sqlca)
ll_Fila = vinf.dw_1.Retrieve(uo_SelCliente.Codigo, uo_SelPlanta.Codigo, ld_MesProceso, uo_SelEspecie.Codigo, ld_Desde, ld_Hasta, Dec(em_ValorCambio.Text), -1, 0)

IF ll_Fila = -1 THEN
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
ELSEIF ll_Fila = 0 THEN
	MessageBox( "No Existe información", "No existe información para este informe.", StopSign!, Ok!)
ELSE
	IF gs_Ambiente = 'Windows' THEN
		vinf.dw_1.Modify('DataWindow.Print.Preview = Yes')
		vinf.dw_1.Modify('DataWindow.Print.Preview.Zoom = 75')
	
		vinf.Visible	= True
		vinf.Enabled	= True
	ELSE
		F_ImprimeInformePdf(vinf.dw_1, "TituloInforme")
	END IF
//	
//	If MessageBox('Atencion', "Esta correcta la validacion?", Exclamation!, YesNo!, 2) = 1 Then
		pb_Acepta.Enabled = True
//	End If
END IF
end event


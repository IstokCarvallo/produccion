$PBExportHeader$w_procesa_termografo.srw
forward
global type w_procesa_termografo from window
end type
type rb_normal from radiobutton within w_procesa_termografo
end type
type rb_multiuso from radiobutton within w_procesa_termografo
end type
type st_17 from statictext within w_procesa_termografo
end type
type em_9 from editmask within w_procesa_termografo
end type
type st_16 from statictext within w_procesa_termografo
end type
type em_8 from editmask within w_procesa_termografo
end type
type st_15 from statictext within w_procesa_termografo
end type
type st_14 from statictext within w_procesa_termografo
end type
type em_7 from editmask within w_procesa_termografo
end type
type em_6 from editmask within w_procesa_termografo
end type
type em_5 from editmask within w_procesa_termografo
end type
type st_13 from statictext within w_procesa_termografo
end type
type st_12 from statictext within w_procesa_termografo
end type
type em_4 from editmask within w_procesa_termografo
end type
type st_11 from statictext within w_procesa_termografo
end type
type em_3 from editmask within w_procesa_termografo
end type
type st_10 from statictext within w_procesa_termografo
end type
type em_2 from editmask within w_procesa_termografo
end type
type st_8 from statictext within w_procesa_termografo
end type
type em_1 from editmask within w_procesa_termografo
end type
type st_9 from statictext within w_procesa_termografo
end type
type st_7 from statictext within w_procesa_termografo
end type
type dw_11 from datawindow within w_procesa_termografo
end type
type dw_10 from datawindow within w_procesa_termografo
end type
type st_4 from statictext within w_procesa_termografo
end type
type st_3 from statictext within w_procesa_termografo
end type
type em_fzarpe from editmask within w_procesa_termografo
end type
type em_operacion from editmask within w_procesa_termografo
end type
type st_5 from statictext within w_procesa_termografo
end type
type st_2 from statictext within w_procesa_termografo
end type
type pb_salir from picturebutton within w_procesa_termografo
end type
type pb_grabar from picturebutton within w_procesa_termografo
end type
type st_1 from statictext within w_procesa_termografo
end type
type gb_3 from groupbox within w_procesa_termografo
end type
type st_6 from statictext within w_procesa_termografo
end type
end forward

global type w_procesa_termografo from window
integer width = 3017
integer height = 1764
boolean titlebar = true
boolean controlmenu = true
windowtype windowtype = response!
long backcolor = 16777215
string icon = "TABLA.ICO"
event ue_guardar ( )
event ue_buscar pbm_custom12
event ue_ordenar pbm_custom13
event ue_carga_detalle pbm_custom27
event ue_listo ( )
event ue_antesguardar pbm_custom75
event ue_seleccion pbm_custom17
event ue_imprimir pbm_custom03
event ue_genera_facturas pbm_custom04
rb_normal rb_normal
rb_multiuso rb_multiuso
st_17 st_17
em_9 em_9
st_16 st_16
em_8 em_8
st_15 st_15
st_14 st_14
em_7 em_7
em_6 em_6
em_5 em_5
st_13 st_13
st_12 st_12
em_4 em_4
st_11 st_11
em_3 em_3
st_10 st_10
em_2 em_2
st_8 st_8
em_1 em_1
st_9 st_9
st_7 st_7
dw_11 dw_11
dw_10 dw_10
st_4 st_4
st_3 st_3
em_fzarpe em_fzarpe
em_operacion em_operacion
st_5 st_5
st_2 st_2
pb_salir pb_salir
pb_grabar pb_grabar
st_1 st_1
gb_3 gb_3
st_6 st_6
end type
global w_procesa_termografo w_procesa_termografo

type variables
str_mant               istr_mant
str_busqueda           istr_busq
Date		id_FechaAcceso
Time		it_HoraAcceso
integer  ii_var, ii_cal, ii_prod, ii_packing
String	is_archivo

DataWindowChild	idwc_cliente, idwc_planta
end variables

forward prototypes
public function boolean existeguia (integer cliente, integer planta, long numero, string embarque)
protected function boolean wf_actualiza_db (boolean borrando)
public function boolean validadespacho (integer li_cliente, integer li_planta, long ll_guia)
public subroutine enviamail ()
public function boolean existetermografo (string as_codigo)
protected function boolean existedespacho (integer li_cliente, integer li_planta)
end prototypes

event ue_guardar();Integer li_codigo

SetPointer(HourGlass!)

w_main.SetMicroHelp("Grabando información...")

IF wf_actualiza_db(False) THEN
	w_main.SetMicroHelp("Información Grabada.")
	pb_grabar.Enabled = False
	em_operacion.Text = ''
	em_operacion.SetFocus()
ELSE
	w_main.SetMicroHelp("No se puede Grabar información.")
	Message.DoubleParm = -1
	RETURN
END IF 
end event

event ue_listo;w_main.SetMicroHelp("Listo")
SetPointer(Arrow!)

end event

public function boolean existeguia (integer cliente, integer planta, long numero, string embarque);Long li_Existe
	
  SELECT defe_guides  
    INTO :li_existe  
    FROM dbo.despafrigoen  
   WHERE clie_codigo = :cliente AND  
         plde_codigo = :planta  AND  
         defe_guides = :numero  AND
			embq_codigo = :embarque;

	IF sqlca.SQLCode = -1 THEN
		F_ErrorBaseDatos(sqlca,"Lectura de la Tabla despafrigoen")
	
		RETURN False
	ELSEIF sqlca.SQLCode = 100 THEN
		MessageBox("Atención", "No existe Guía Despacho Indicada.~r~rIngrese otro Número.", &
						Exclamation!, Ok!)
		pb_grabar.Enabled	= False
	
		RETURN False
	ELSE
		pb_grabar.Enabled	= True
		RETURN True
	END IF



end function

protected function boolean wf_actualiza_db (boolean borrando);date		ld_fecha
String	ls_receptor, ls_observacion, ls_termografo
Long		ll_numero
Integer	li_cliente, li_planta

li_cliente 		= dw_10.Object.clie_codigo[1]
li_planta  		= dw_11.Object.plde_codigo[1]
ls_termografo	= em_operacion.Text
ll_numero		= Long(em_1.Text)
ld_fecha			= Date(em_4.Text)
ls_receptor		= em_8.Text
ls_observacion = em_9.Text

UPDATE dbo.movitermografo SET
mter_fretor = :ld_fecha,
mter_recept = :ls_receptor,
mter_observ = :ls_observacion
WHERE clie_codigo = :li_cliente
AND	plde_codigo = :li_planta
AND	term_codigo = :ls_termografo
AND	term_tipter = 2
AND	defe_numero = :ll_numero;

commit;
IF sqlca.SQLCode = -1 THEN
	F_errorbasedatos(sqlca,"Lectura tabla movitermografo")
	RETURN False
END IF

UPDATE dbo.termografo SET
term_estado = 1
WHERE clie_codigo = :li_cliente
AND	plde_codigo = :li_planta
AND	term_codigo = :ls_termografo
AND	term_tipter = 2;

commit;
IF sqlca.SQLCode = -1 THEN
	F_errorbasedatos(sqlca,"Lectura tabla termografo")
	RETURN False
END IF


MessageBox("Atención", "Retorno de Termógrafo Realizado con Exito.", &
					Exclamation!, Ok!)

Return True
end function

public function boolean validadespacho (integer li_cliente, integer li_planta, long ll_guia);Long ll_numero
Integer li_contador, li_count, li_code_gengde, li_code_gemide, li_code_demade
Date ld_fecha
Time lt_hora
String	ls_embarque

ls_embarque = String(em_operacion.Text)

SELECT defe_numero
INTO	:ll_numero
FROM	dbo.DESPAFRIGOEN 
WHERE	plde_codigo =	:li_planta
AND	clie_codigo	=	:li_cliente
AND	defe_guides	=	:ll_guia
AND   embq_codigo = :ls_embarque;

IF sqlca.SQLCode = -1 THEN
		F_ErrorBaseDatos(sqlca,"Lectura de la Tabla DESPAFRIGOEN")
		RETURN True
	ELSEIF sqlca.SQLCode = 100 THEN
		MessageBox("Atención", "No existe Número Guia Despacho Indicado.~r~rIngrese otro Número.", &
						Exclamation!, Ok!)
		RETURN True
END IF

SELECT count(*),max(CODE_FECHAA),max(CODE_HORAAP)
INTO :li_contador,:ld_fecha,:lt_hora
FROM dbo.CONTROLDESPACHOS
where plde_codigo =	:li_planta
AND	clie_codigo	=	:li_cliente
AND	defe_numero	=	:ll_numero;

IF sqlca.SQLCode = -1 THEN
		F_ErrorBaseDatos(sqlca,"Lectura de la Tabla CONTROLDESPACHOS")
		RETURN True
END IF

IF li_contador > 0 THEN
	Select CODE_GENGDE 
	INTO :li_code_gengde
	from dbo.CONTROLDESPACHOS
	where  plde_codigo =	:li_planta
		AND	clie_codigo =	:li_cliente
		AND	defe_numero =	:ll_numero
		AND	CODE_FECHAA =  :ld_fecha;
		
	IF Isnull(li_code_gengde) OR li_code_gengde = 0 THEN
		MessageBox("Atención", "Falta Generar Informe de Guía de Despacho.", &
					Exclamation!, Ok!)
		Return True
	END IF		
		
END IF	

IF li_contador > 0 THEN
	Select CODE_GEMIDE 
	INTO :li_code_gemide
	from dbo.CONTROLDESPACHOS
	where  plde_codigo =	:li_planta
		AND	clie_codigo =	:li_cliente
		AND	defe_numero =	:ll_numero
		AND	CODE_FECHAA =  :ld_fecha;
		
	IF Isnull(li_code_gemide) OR li_code_gemide = 0 THEN
		MessageBox("Atención", "Falta Generar Impresión de Despacho.", &
					Exclamation!, Ok!)
		Return True
	END IF		
	
END IF	

IF li_contador > 0 THEN
	Select CODE_DEMADE 
	INTO :li_code_demade
	from dbo.CONTROLDESPACHOS
	where  plde_codigo =	:li_planta
		AND	clie_codigo =	:li_cliente
		AND	defe_numero =	:ll_numero
		AND	CODE_FECHAA =  :ld_fecha;
	
	IF Isnull(li_code_demade) OR li_code_demade = 0 THEN
		MessageBox("Atención", "Falta Generar Listado Anexo de Despacho.", &
					Exclamation!, Ok!)
		Return True
	END IF		
	
END IF	

return False			
			

end function

public subroutine enviamail ();String		ls_Nombre, ls_NomReporte, ls_Archivo, ls_DirectorioAct 
Long			ll_Fila, ll_consignatario, ll_Archivo
Boolean		lb_Existe
Integer		li_imprimio
str_parms	lstr_parms

SetPointer(HourGlass!)

ls_NomReporte									=	is_archivo
lstr_parms.string_arg[1]					=	String(1)
lstr_parms.string_arg[2]					=	is_archivo
lstr_parms.string_arg[3]					=	String(1)

ll_Archivo = 1

lstr_parms.string_arg[ll_Archivo+3]		=	gs_disco+":\GeneradosSAAM\" + is_archivo //+ ".txt"

OpenWithParm(w_correo_archivo_saam, lstr_parms)

SetPointer(Arrow!)
end subroutine

public function boolean existetermografo (string as_codigo);Integer	li_codexp
String	li_count

SELECT	count(*)
	INTO	:li_count
	FROM	dbo.termografo
	WHERE	term_codigo	= :as_codigo
	AND	term_tipter = 2
	AND	term_estado = 2;
			
IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca,"Lectura de la Tabla Termografo")
	em_operacion.SetFocus()
	RETURN False
ELSEIF li_count = '0' THEN
	MessageBox("Atención", "No existe Termógrafo o No Esta en Tránsito.~r~rIngrese otro Termógrafo.", &
					Exclamation!, Ok!)
	pb_grabar.Enabled	= False
	em_operacion.SetFocus()
	RETURN False
ELSE
	pb_grabar.Enabled	= True
	RETURN True
END IF

end function

protected function boolean existedespacho (integer li_cliente, integer li_planta);Integer 	li_tipo, li_transporte
Long 		ll_numero, ll_despamax
String	ls_embarque, ls_patente, ls_chofer, ls_termografo, ls_transportista
date		ld_fecha

ls_termografo = String(em_operacion.Text)

SELECT MAX(defe_numero)
INTO	:ll_despamax
FROM	dbo.DESPAFRIGOEN 
WHERE	plde_codigo =	:li_planta
AND	clie_codigo	=	:li_cliente
AND   (defe_term01 = :ls_termografo OR defe_term02 = :ls_termografo OR defe_term03 = :ls_termografo)
;

SELECT defe_numero,defe_tiposa,embq_codigo,defe_fecdes,tran_codigo,defe_patent,defe_chofer
INTO	:ll_numero,:li_tipo,:ls_embarque,:ld_fecha,:li_transporte,:ls_patente,:ls_chofer
FROM	dbo.DESPAFRIGOEN 
WHERE	plde_codigo =	:li_planta
AND   defe_numero =  :ll_despamax
AND	clie_codigo	=	:li_cliente
AND   (defe_term01 = :ls_termografo OR defe_term02 = :ls_termografo OR defe_term03 = :ls_termografo);

IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca,"Lectura de la Tabla DESPAFRIGOEN")
	RETURN True
ELSEIF sqlca.SQLCode = 100 THEN
	MessageBox("Atención", "No existe Número Termógrafo Despachado.~r~rIngrese otro Número.", &
					Exclamation!, Ok!)
	RETURN True
END IF

em_1.Text 		= String(ll_numero)
em_2.Text 		= String(li_tipo)
em_3.Text 		= ls_embarque
em_6.Text 		= ls_patente
em_7.Text 		= ls_chofer
em_fzarpe.Text = String(ld_fecha)

SELECT tran_nombre
INTO :ls_transportista
FROM dbo.transportista
where tran_codigo =	:li_transporte;

IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca,"Lectura de la Tabla transportista")
	RETURN True
ELSE
	em_5.Text = ls_transportista
END IF

IF li_tipo = 2 THEN
  	em_2.Text = '2. Reproceso'
ELSEIF li_tipo = 3 THEN
  	em_2.Text = '3. Reembalaje'
ELSEIF li_tipo = 5 THEN 
  	em_2.Text = '5. Reproceso Serv. 3º'
ELSEIF li_tipo = 6 THEN 
  	em_2.Text = '6. Reembalaje Serv.3ª'
ELSEIF li_tipo = 7 THEN
	em_2.Text = '7. Embarque Marítimo'
ELSEIF li_tipo = 8 THEN	
	em_2.Text = '8. Embarque Aereo'
ELSEIF li_tipo = 9 THEN
	em_2.Text = '9. Embarque Terrestre'
ELSEIF li_tipo = 10 THEN	
  	em_2.Text = '10. Devolución al Productor'
ELSEIF li_tipo = 11 THEN
  	em_2.Text = '11. Traspaso Inter-Planta'
ELSEIF li_tipo = 12 THEN  
  	em_2.Text = '12.  M/I Cta. Propia'
ELSEIF li_tipo = 13 THEN  
	em_2.Text = '13. M/I Cta. Productor'
ELSEIF li_tipo = 14 THEN	
	em_2.Text = '14. Muestra Ensayo'
ELSEIF li_tipo = 15 THEN	
	em_2.Text = '15. Botadero'
ELSEIF li_tipo = 16 THEN	
	em_2.Text = '16. Ventas No Fact.Prod'
ELSEIF li_tipo = 17 THEN	
	em_2.Text = '17. Desp. Servicio 3º'
ELSEIF li_tipo = 20 THEN	
	em_2.Text = '20. Packing Externo'
ELSEIF li_tipo = 21 THEN	
	em_2.Text = '21. Venta Exp. País'
ELSEIF li_tipo = 30 THEN	
	em_2.Text = '30.Sitio de Revisión'
ELSEIF li_tipo = 31 THEN
	em_2.Text = '31. Despacho por Cajas'
END IF	

Return FALSE




	
end function

on w_procesa_termografo.create
this.rb_normal=create rb_normal
this.rb_multiuso=create rb_multiuso
this.st_17=create st_17
this.em_9=create em_9
this.st_16=create st_16
this.em_8=create em_8
this.st_15=create st_15
this.st_14=create st_14
this.em_7=create em_7
this.em_6=create em_6
this.em_5=create em_5
this.st_13=create st_13
this.st_12=create st_12
this.em_4=create em_4
this.st_11=create st_11
this.em_3=create em_3
this.st_10=create st_10
this.em_2=create em_2
this.st_8=create st_8
this.em_1=create em_1
this.st_9=create st_9
this.st_7=create st_7
this.dw_11=create dw_11
this.dw_10=create dw_10
this.st_4=create st_4
this.st_3=create st_3
this.em_fzarpe=create em_fzarpe
this.em_operacion=create em_operacion
this.st_5=create st_5
this.st_2=create st_2
this.pb_salir=create pb_salir
this.pb_grabar=create pb_grabar
this.st_1=create st_1
this.gb_3=create gb_3
this.st_6=create st_6
this.Control[]={this.rb_normal,&
this.rb_multiuso,&
this.st_17,&
this.em_9,&
this.st_16,&
this.em_8,&
this.st_15,&
this.st_14,&
this.em_7,&
this.em_6,&
this.em_5,&
this.st_13,&
this.st_12,&
this.em_4,&
this.st_11,&
this.em_3,&
this.st_10,&
this.em_2,&
this.st_8,&
this.em_1,&
this.st_9,&
this.st_7,&
this.dw_11,&
this.dw_10,&
this.st_4,&
this.st_3,&
this.em_fzarpe,&
this.em_operacion,&
this.st_5,&
this.st_2,&
this.pb_salir,&
this.pb_grabar,&
this.st_1,&
this.gb_3,&
this.st_6}
end on

on w_procesa_termografo.destroy
destroy(this.rb_normal)
destroy(this.rb_multiuso)
destroy(this.st_17)
destroy(this.em_9)
destroy(this.st_16)
destroy(this.em_8)
destroy(this.st_15)
destroy(this.st_14)
destroy(this.em_7)
destroy(this.em_6)
destroy(this.em_5)
destroy(this.st_13)
destroy(this.st_12)
destroy(this.em_4)
destroy(this.st_11)
destroy(this.em_3)
destroy(this.st_10)
destroy(this.em_2)
destroy(this.st_8)
destroy(this.em_1)
destroy(this.st_9)
destroy(this.st_7)
destroy(this.dw_11)
destroy(this.dw_10)
destroy(this.st_4)
destroy(this.st_3)
destroy(this.em_fzarpe)
destroy(this.em_operacion)
destroy(this.st_5)
destroy(this.st_2)
destroy(this.pb_salir)
destroy(this.pb_grabar)
destroy(this.st_1)
destroy(this.gb_3)
destroy(this.st_6)
end on

event open;x = 0
y = 0

This.Icon	=	Gstr_apl.Icono

dw_10.GetChild("clie_codigo", idwc_cliente)
idwc_cliente.SetTransObject(SQLCA)
idwc_cliente.Retrieve()
dw_10.InsertRow(0)
dw_10.SetItem(1,"clie_codigo",gi_codexport)

dw_11.GetChild("plde_codigo", idwc_planta)
idwc_planta.SetTransObject(sqlca)
idwc_planta.Retrieve(1)
dw_11.InsertRow(0)
dw_11.SetItem(1,"plde_codigo",gi_codplanta)

istr_mant.argumento[1]	=	String(gi_codexport)
istr_mant.argumento[2]	=	String(gi_codplanta)

em_4.Text = String(Today())

GrabaAccesoAplicacion(True, id_FechaAcceso, it_HoraAcceso, &
							This.Title, "Acceso a Aplicación", 1)

end event

event close;GrabaAccesoAplicacion(False, id_FechaAcceso, it_HoraAcceso, "", "", 0)

end event

type rb_normal from radiobutton within w_procesa_termografo
integer x = 1481
integer y = 272
integer width = 402
integer height = 80
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Normal"
end type

type rb_multiuso from radiobutton within w_procesa_termografo
integer x = 795
integer y = 272
integer width = 402
integer height = 80
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "MultiUso"
boolean checked = true
end type

type st_17 from statictext within w_procesa_termografo
integer x = 96
integer y = 248
integer width = 2487
integer height = 124
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 16777215
long backcolor = 16711680
boolean enabled = false
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type em_9 from editmask within w_procesa_termografo
integer x = 731
integer y = 900
integer width = 1774
integer height = 92
integer taborder = 60
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long backcolor = 16777215
textcase textcase = upper!
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = stringmask!
string displaydata = "~t/"
end type

event getfocus;//IF ExisteOperacion(This.Text) 
end event

type st_16 from statictext within w_procesa_termografo
integer x = 201
integer y = 924
integer width = 439
integer height = 64
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
boolean enabled = false
string text = "Observaciones"
boolean focusrectangle = false
end type

type em_8 from editmask within w_procesa_termografo
integer x = 731
integer y = 776
integer width = 1774
integer height = 92
integer taborder = 50
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long backcolor = 16777215
textcase textcase = upper!
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = stringmask!
string displaydata = "~t/"
end type

event getfocus;//IF ExisteOperacion(This.Text) 
end event

type st_15 from statictext within w_procesa_termografo
integer x = 201
integer y = 800
integer width = 471
integer height = 64
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
boolean enabled = false
string text = "Receptor"
boolean focusrectangle = false
end type

type st_14 from statictext within w_procesa_termografo
integer x = 192
integer y = 1472
integer width = 352
integer height = 64
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Chofer"
boolean focusrectangle = false
end type

type em_7 from editmask within w_procesa_termografo
integer x = 603
integer y = 1468
integer width = 832
integer height = 92
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
alignment alignment = center!
textcase textcase = upper!
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = stringmask!
string displaydata = "~t/"
end type

event getfocus;//IF ExisteOperacion(This.Text) 
end event

event modified;//IF ExisteOperacion(This.Text) = False THEN
//	This.SetFocus()
//END IF
//


 
	
end event

type em_6 from editmask within w_procesa_termografo
integer x = 2034
integer y = 1356
integer width = 471
integer height = 92
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
alignment alignment = center!
textcase textcase = upper!
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = stringmask!
string displaydata = "~t/"
end type

event getfocus;//IF ExisteOperacion(This.Text) 
end event

event modified;//IF ExisteOperacion(This.Text) = False THEN
//	This.SetFocus()
//END IF



 
	
end event

type em_5 from editmask within w_procesa_termografo
integer x = 603
integer y = 1356
integer width = 1193
integer height = 92
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
textcase textcase = upper!
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = stringmask!
string displaydata = "~t/"
end type

event getfocus;//IF ExisteOperacion(This.Text) 
end event

event modified;//IF ExisteOperacion(This.Text) = False THEN
//	This.SetFocus()
//END IF
//


 
	
end event

type st_13 from statictext within w_procesa_termografo
integer x = 1801
integer y = 1364
integer width = 256
integer height = 64
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Patente"
boolean focusrectangle = false
end type

type st_12 from statictext within w_procesa_termografo
integer x = 192
integer y = 1364
integer width = 389
integer height = 64
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Trasportista"
boolean focusrectangle = false
end type

type em_4 from editmask within w_procesa_termografo
integer x = 2094
integer y = 652
integer width = 402
integer height = 92
integer taborder = 40
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long backcolor = 16777215
alignment alignment = center!
boolean displayonly = true
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = datemask!
string mask = "dd/mm/yyyy"
string displaydata = "@"
end type

type st_11 from statictext within w_procesa_termografo
integer x = 1138
integer y = 1256
integer width = 443
integer height = 64
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Fecha"
boolean focusrectangle = false
end type

type em_3 from editmask within w_procesa_termografo
integer x = 603
integer y = 1244
integer width = 421
integer height = 92
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
alignment alignment = center!
textcase textcase = upper!
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = stringmask!
string displaydata = "~t/"
end type

event getfocus;//IF ExisteOperacion(This.Text) 
end event

event modified;//IF ExisteOperacion(This.Text) = False THEN
//	This.SetFocus()
//END IF
//
//
//
 
	
end event

type st_10 from statictext within w_procesa_termografo
integer x = 192
integer y = 1256
integer width = 311
integer height = 64
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Instructivo"
boolean focusrectangle = false
end type

type em_2 from editmask within w_procesa_termografo
integer x = 1591
integer y = 1132
integer width = 914
integer height = 92
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
textcase textcase = upper!
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = stringmask!
string displaydata = "  2. Reproceso~t2/  3. Reembalaje~t3/  5. Reproceso Serv. 3º~t5/  6. Reembalaje Serv.3ª~t6/  7. Embarque Marítimo~t7/  8. Embarque Aereo~t8/  9. Embarque Terrestre~t9/10. Devolución al Productor~t10/11. Traspaso Inter-Planta~t11/12.  M//I Cta. Propia~t12/13. M//I Cta. Productor~t13/14. Muestra Ensayo~t14/15. Botadero~t15/16. Ventas No Fact.Prod~t16/17. Desp. Servicio 3º~t17/20. Packing Externo~t20/21. Venta Exp. País~t21/30.Sitio de Revisión~t30/31. Despacho por Cajas~t31/"
end type

event getfocus;//IF ExisteOperacion(This.Text) 
end event

type st_8 from statictext within w_procesa_termografo
integer x = 1138
integer y = 1148
integer width = 443
integer height = 64
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Tipo Despacho"
boolean focusrectangle = false
end type

type em_1 from editmask within w_procesa_termografo
integer x = 603
integer y = 1132
integer width = 421
integer height = 92
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
alignment alignment = center!
textcase textcase = upper!
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = stringmask!
string displaydata = "~t/"
end type

event getfocus;//IF ExisteOperacion(This.Text) 
end event

event modified;//IF ExisteOperacion(This.Text) = False THEN
//	This.SetFocus()
//END IF
//


 
	
end event

type st_9 from statictext within w_procesa_termografo
integer x = 192
integer y = 1148
integer width = 265
integer height = 64
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Número"
boolean focusrectangle = false
end type

type st_7 from statictext within w_procesa_termografo
integer x = 1650
integer y = 664
integer width = 439
integer height = 64
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Fecha Retorno"
boolean focusrectangle = false
end type

type dw_11 from datawindow within w_procesa_termografo
integer x = 731
integer y = 528
integer width = 969
integer height = 100
integer taborder = 20
boolean bringtotop = true
string title = "none"
string dataobject = "dddw_plantadesp_tipo"
boolean border = false
boolean livescroll = true
end type

event itemchanged;istr_mant.argumento[2]	=	data
end event

type dw_10 from datawindow within w_procesa_termografo
integer x = 731
integer y = 404
integer width = 1225
integer height = 96
integer taborder = 10
boolean bringtotop = true
string dataobject = "dddw_clientesprod"
boolean border = false
boolean livescroll = true
end type

event itemchanged;istr_mant.argumento[1]	=	data
idwc_planta.Retrieve(1)
istr_mant.argumento[2]	=	String(dw_11.Object.plde_codigo[1])
dw_11.SetItem(1, "plde_codigo", Integer(istr_mant.argumento[2]))

end event

type st_4 from statictext within w_procesa_termografo
integer x = 201
integer y = 552
integer width = 402
integer height = 64
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

type st_3 from statictext within w_procesa_termografo
integer x = 201
integer y = 428
integer width = 402
integer height = 64
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

type em_fzarpe from editmask within w_procesa_termografo
integer x = 1591
integer y = 1244
integer width = 439
integer height = 92
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
alignment alignment = center!
boolean displayonly = true
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = datemask!
string mask = "dd/mm/yyyy"
string displaydata = "@"
end type

type em_operacion from editmask within w_procesa_termografo
event getfocus pbm_ensetfocus
event modified pbm_enmodified
integer x = 731
integer y = 656
integer width = 635
integer height = 92
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
textcase textcase = upper!
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = stringmask!
string displaydata = "~t/"
end type

event getfocus;//IF ExisteOperacion(This.Text) 
end event

event modified;
IF This.Text <> '' THEN
	IF rb_multiuso.Checked THEN
		IF existetermografo(This.Text) = False THEN
			This.Text	= ''
			This.SetFocus()
			Return 1
		ELSE
			IF existedespacho(dw_10.Object.clie_codigo[1],dw_11.Object.plde_codigo[1]) THEN
				This.Text	= ''
				This.SetFocus()
				Return 1
			END IF	
		END IF
	ELSE
		MessageBox("Atención", "No existe Termógrafo o No Esta en Tránsito.~r~rIngrese otro Termógrafo.", &
						Exclamation!, Ok!)
		This.Text	= ''
		This.SetFocus()
		Return 1					
	END IF	
END IF	



 
	
end event

type st_5 from statictext within w_procesa_termografo
integer x = 78
integer y = 68
integer width = 2487
integer height = 92
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 16711680
boolean enabled = false
string text = "RETORNO TERMOGRAFO"
alignment alignment = center!
boolean border = true
borderstyle borderstyle = stylelowered!
boolean focusrectangle = false
end type

type st_2 from statictext within w_procesa_termografo
integer x = 201
integer y = 676
integer width = 471
integer height = 64
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
boolean enabled = false
string text = "Nro Termógrafo"
boolean focusrectangle = false
end type

type pb_salir from picturebutton within w_procesa_termografo
integer x = 2670
integer y = 1320
integer width = 302
integer height = 244
integer taborder = 80
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean cancel = true
string picturename = "\Desarrollo 17\Imagenes\Botones\Apagar.png"
alignment htextalign = left!
long backcolor = 553648127
end type

event clicked;Close(Parent)
end event

type pb_grabar from picturebutton within w_procesa_termografo
integer x = 2674
integer y = 1060
integer width = 302
integer height = 244
integer taborder = 70
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean enabled = false
boolean default = true
string picturename = "\Desarrollo 17\Imagenes\Botones\Guardar Como.png"
string disabledname = "\Desarrollo 17\Imagenes\Botones\Guardar Como-bn.png"
alignment htextalign = left!
long backcolor = 553648127
end type

event clicked;IF em_8.Text = '' THEN
	MessageBox("Atención", "Falta Ingreso de Receptor.", &
					Exclamation!, Ok!)
	em_8.SetFocus()					
	RETURN 1
END IF	

IF em_9.Text = '' THEN
	MessageBox("Atención", "Falta Ingreso de Observaciones.", &
					Exclamation!, Ok!)
	em_9.SetFocus()						
	RETURN 1
END IF	

Parent.TriggerEvent("ue_guardar")
end event

type st_1 from statictext within w_procesa_termografo
integer x = 96
integer y = 372
integer width = 2487
integer height = 644
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 16777215
long backcolor = 16711680
boolean enabled = false
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type gb_3 from groupbox within w_procesa_termografo
integer x = 142
integer y = 1032
integer width = 2391
integer height = 564
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Datos Despacho"
end type

type st_6 from statictext within w_procesa_termografo
integer x = 96
integer y = 1016
integer width = 2487
integer height = 620
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 16777215
long backcolor = 16711680
boolean enabled = false
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type


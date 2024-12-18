$PBExportHeader$w_gene_archivo_saam.srw
forward
global type w_gene_archivo_saam from window
end type
type dw_6 from datawindow within w_gene_archivo_saam
end type
type dw_7 from datawindow within w_gene_archivo_saam
end type
type dw_4 from datawindow within w_gene_archivo_saam
end type
type dw_5 from datawindow within w_gene_archivo_saam
end type
type dw_guia from uo_dw within w_gene_archivo_saam
end type
type cbx_resumen from checkbox within w_gene_archivo_saam
end type
type uo_selplanta from uo_seleccion_plantas within w_gene_archivo_saam
end type
type uo_selcliente from uo_seleccion_clientesprod within w_gene_archivo_saam
end type
type cbx_pak from checkbox within w_gene_archivo_saam
end type
type st_7 from statictext within w_gene_archivo_saam
end type
type em_nroguia from editmask within w_gene_archivo_saam
end type
type cbx_prod from checkbox within w_gene_archivo_saam
end type
type cbx_cal from checkbox within w_gene_archivo_saam
end type
type cbx_var from checkbox within w_gene_archivo_saam
end type
type st_4 from statictext within w_gene_archivo_saam
end type
type st_3 from statictext within w_gene_archivo_saam
end type
type sle_mensa from singlelineedit within w_gene_archivo_saam
end type
type em_fzarpe from editmask within w_gene_archivo_saam
end type
type sle_nave from singlelineedit within w_gene_archivo_saam
end type
type st_5 from statictext within w_gene_archivo_saam
end type
type pb_salir from picturebutton within w_gene_archivo_saam
end type
type pb_grabar from picturebutton within w_gene_archivo_saam
end type
type st_6 from statictext within w_gene_archivo_saam
end type
type st_1 from statictext within w_gene_archivo_saam
end type
end forward

global type w_gene_archivo_saam from window
integer width = 2496
integer height = 1320
boolean titlebar = true
boolean controlmenu = true
windowtype windowtype = response!
long backcolor = 16777215
string icon = "AppIcon!"
windowanimationstyle openanimation = topslide!
event ue_guardar ( )
event ue_buscar pbm_custom12
event ue_ordenar pbm_custom13
event ue_carga_detalle pbm_custom27
event ue_listo ( )
event ue_antesguardar pbm_custom75
event ue_seleccion pbm_custom17
event ue_imprimir pbm_custom03
event ue_genera_facturas pbm_custom04
dw_6 dw_6
dw_7 dw_7
dw_4 dw_4
dw_5 dw_5
dw_guia dw_guia
cbx_resumen cbx_resumen
uo_selplanta uo_selplanta
uo_selcliente uo_selcliente
cbx_pak cbx_pak
st_7 st_7
em_nroguia em_nroguia
cbx_prod cbx_prod
cbx_cal cbx_cal
cbx_var cbx_var
st_4 st_4
st_3 st_3
sle_mensa sle_mensa
em_fzarpe em_fzarpe
sle_nave sle_nave
st_5 st_5
pb_salir pb_salir
pb_grabar pb_grabar
st_6 st_6
st_1 st_1
end type
global w_gene_archivo_saam w_gene_archivo_saam

type variables
str_busqueda		istr_busq
Str_info				istr_info

uo_Despachos			iuo_Despacho
uo_Puertos				iuo_Puertos
uo_Embalajes			iuo_Embalaje
uo_Envases				iuo_Envase
uo_EmbarquesProd 	iuo_Embarque
uo_GuiaDespacho		iuo_Guia

Date			id_FechaAcceso, id_fecha
Time			it_HoraAcceso
integer  		ii_var, ii_cal, ii_prod, ii_packing, ii_especie, ii_condicion
String			is_archivoguias, is_archivo, is_report, is_embarque
Long			al_planilla, il_despacho

w_informes_guia					vinf1
w_informes_guia_archivo 		vinf2
end variables

forward prototypes
public subroutine wf_generaarchivo ()
public function boolean wf_obtienecondicion (integer ai_cliente, integer ai_planta, long al_pallet)
public function boolean wf_ValidaDatos ()
public function boolean wf_existeguia (long al_guia)
public function boolean wf_existearchivo (integer li_cliente, integer li_planta, long ll_guia, long al_planillas, string as_tipoplanilla, string as_instructivo)
end prototypes

public subroutine wf_generaarchivo ();Long			ll_fila, ll_filas, ll_filadet, ll_guia, ll_numero, ll_cont, ll_planilla, ll_filcont
String			ls_Cliente, ls_Planta, ls_Patente, ls_Archivo, ls_Registro, ls_embarque,&
				ls_termog, ls_guia, ls_Instructivo
double		ll_termog
Integer		li_copallet, li_cliente,  li_Tipova, li_Planta
Datetime    	ldt_hora

ldt_hora			=	Datetime(Date(Today()),Time(Today())	)

dw_6.reset()
dw_7.reset()
dw_4.reset()
dw_5.reset()

dw_7.SetTransObject(Sqlca)
dw_6.SetTransObject(Sqlca)

// Para actualizar el estado=1 Cerrado, una vez generado el archivo
dw_4.SetTransObject(SQLCA)
dw_5.SetTransObject(SQLCA)
dw_guia.SetTransObject(SQLCA)


If IsNull(al_planilla) Then al_planilla = 0

ls_Cliente		=	String(uo_SelCliente.Codigo, '000')
ls_Planta			=	String(uo_SelPlanta.Codigo, '0000')
ls_guia 			=  em_nroguia.Text
ll_guia 			=  Long(em_nroguia.Text)
ls_Instructivo	=	is_embarque
ls_embarque	=	is_embarque
ll_planilla			=	al_planilla

ls_Archivo	=  "RIOB" +ls_Instructivo+"-"+ string(ll_planilla) + "." + Mid(ls_Planta, 2, 3) + ls_guia
is_archivo	=	ls_Archivo

ll_filcont		=  dw_5.Retrieve(uo_SelCliente.Codigo,uo_SelPlanta.Codigo,ll_planilla,ii_var,ii_cal,ii_prod,ll_guia, 0, '-1',ls_Instructivo)

If ll_filcont > 0 Then
	If wf_ValidaDatos() Then
		Return
	End If	
End If	

ll_filas	=  dw_6.Retrieve(uo_SelCliente.Codigo,uo_SelPlanta.Codigo,ll_planilla,ii_var,ii_cal,ii_prod,ll_guia, 0, '-1',ls_Instructivo)
									  
li_cliente = uo_SelCliente.Codigo

If ll_filas = -1 Then
	F_ErrorBaseDatos(sqlca,"Lectura de la Tabla EMBARQUEPROD")
ElseIf ll_filas = 0 Then
	MessageBox("Atención", "No hay información con Operación Indicada.~r~rIngrese otra Operación.", Exclamation!, Ok!)
Else
	dw_6.SetSort('paen_numero')
     dw_6.Sort()

	FOR ll_fila = 1 TO ll_filas
		ls_Patente	=	String(dw_6.Object.defe_patent[ll_fila])
		
		If IsNull(ls_Patente) Then ls_Patente = ''
		
		ls_Registro	=	dw_6.Object.embq_codigo[ll_fila]
		ls_Registro	+=	String(dw_6.Object.defe_guides[ll_fila], '00000000')
		ls_Registro	+=	String(dw_6.Object.reci_codigo[ll_fila], '00000')
		ls_Registro	+=	String(dw_6.Object.plde_codigo[ll_fila], '0000')
		ls_Registro	+=	ls_Patente + Fill(' ',6 - Len(ls_Patente))
		ls_Registro	+=	String(dw_6.Object.espe_codigo[ll_fila], '00')
				
		wf_ObtieneCondicion(dw_6.Object.clie_codigo[ll_fila],dw_6.Object.plde_codigo[ll_fila],dw_6.Object.paen_numero[ll_fila])
			
		If ii_condicion = 1 Then
			ls_Registro	+=	'F'
		ElseIf dw_6.Object.paen_inspec[ll_fila] = 0 AND ii_condicion <> 1 AND ii_condicion <> 2  Then
			ls_Registro	+=	'N'
		ElseIf dw_6.Object.paen_inspec[ll_fila] = 1 Then
			ls_Registro += 'I'
		ElseIf ii_condicion = 2 Then
			ls_Registro	+=	'U'
		End If
		
		ls_Registro	+=	ls_Cliente
		ls_Registro	+=	String(dw_6.Object.paen_numero[ll_fila], '0000000')
		ls_Registro	+=	String(dw_6.Object.etiq_codigo[ll_fila], '000')
		ls_Registro	+=	String(dw_6.Object.paen_fecemb[ll_fila], 'ddmmyy')
		ls_Registro	+=	String(dw_6.Object.prod_codigo[ll_fila], '00000')
		ls_Registro	+=	String(dw_6.Object.emba_codigo[ll_fila], '@@@@@@@@@@')
		ls_Registro	+=	String(dw_6.Object.vari_codigo[ll_fila], '0000')
		ls_Registro	+=	String(dw_6.Object.pafr_calibr[ll_fila], '@@@@@')
		ls_Registro	+=	String(dw_6.Object.pafr_ccajas[ll_fila], '0000000')
		ls_Registro	+=	String(dw_6.Object.cate_codigo[ll_fila], '000')
		
		iuo_Embalaje.Existe(dw_6.Object.emba_codigo[ll_fila], False, Sqlca)
		iuo_Envase.Existe(iuo_Embalaje.TipoEnvase, iuo_Embalaje.CodEnvase, True, Sqlca)
		
		If dw_6.Object.tica_codigo[ll_fila] = 1 Then
			ls_Registro	+=	'P'
		ElseIf dw_6.Object.tica_codigo[ll_fila] = 2 Then
			ls_Registro	+=	'C'
		Else
			ls_Registro	+=	'F'
		End If
		
		If iuo_Puertos.Existe(dw_6.Object.embq_ptoori[ll_fila], False, Sqlca) Then
			ls_Registro	+=	String(iuo_Puertos.SAAM, "00")
		End If
		
		ls_Registro +=String(dw_6.Object.packing[ll_fila], '0000')

		If IsNull(dw_6.Object.defe_termog[ll_fila]) Then
			ll_termog = 0
			ls_termog = '0'
		Else
			ls_termog = String(dw_6.Object.defe_termog[ll_fila])
		End If
	   ls_Registro +=String(ls_termog, Fill("@", 15))
		
		If IsNull(dw_6.Object.copa_codigo[ll_fila]) Then
			li_copallet	= 	0
		Else
			li_copallet =	dw_6.Object.copa_codigo[ll_fila]
		End If
		ls_Registro +=String(li_copallet, Fill("0", 3))
		ls_Registro += String(iuo_Envase.IfCO)
		
		ll_filadet	=	dw_7.InsertRow(0)
		dw_7.Object.registro[ll_filadet]	=	ls_Registro

		If wf_ExisteArchivo(uo_SelCliente.Codigo,uo_SelPlanta.Codigo,dw_6.Object.defe_guides[ll_fila],&
			al_planilla,String(-1),ls_Instructivo) Then
			Return
		End If
	NEXT
	
	If dw_7.SaveAs(gs_disco+":\GeneradosSAAM\" + ls_Archivo, Text!, False) = -1 Then
		MessageBox("Atención","No se pudo generar el archivo " + ls_Archivo)
	Else
		MessageBox("Atención", "Archivo " + gs_disco+":\GeneradosSAAM\" + ls_Archivo, Exclamation!, Ok!)
		
		ll_filas		=  dw_4.Retrieve(uo_SelCliente.Codigo, uo_SelPlanta.Codigo,al_planilla,ll_guia,'-1',ls_Instructivo)
													  
		If ll_filas = -1 Then
			F_ErrorBaseDatos(sqlca,"Lectura de la Tabla DESPAFRIGOEN")
		ElseIf ll_filas = 0 Then
			MessageBox("Atención", "No hay información ", Exclamation!, Ok!)
		End If
	End If
End If
end subroutine

public function boolean wf_obtienecondicion (integer ai_cliente, integer ai_planta, long al_pallet);Long li_Existe
Date ld_fecha

ii_condicion = 0
	
  SELECT max(fumi_fecfum)  
    INTO :ld_fecha
    FROM dbo.fumigadet as det,dbo.fumigaenc as enc 
   WHERE det.clie_codigo = :ai_cliente AND  
         det.plde_codigo = :ai_planta  AND  
         det.paen_numero = :al_pallet AND
			det.clie_codigo = enc.clie_codigo AND
			det.plde_codigo = enc.plde_codigo AND
			det.cond_codigo = enc.cond_codigo AND
			det.fumi_numero = enc.fumi_numero
	GROUP BY det.cond_codigo;
	
	SELECT det.cond_codigo  
    INTO :ii_condicion 
    FROM dbo.fumigadet as det,dbo.fumigaenc as enc 
   WHERE det.clie_codigo = :ai_cliente AND  
         det.plde_codigo = :ai_planta  AND  
         det.paen_numero = :al_pallet AND
			det.clie_codigo = enc.clie_codigo AND
			det.plde_codigo = enc.plde_codigo AND
			det.cond_codigo = enc.cond_codigo AND
			det.fumi_numero = enc.fumi_numero AND
			enc.fumi_fecfum = :ld_fecha;
	
	IF sqlca.SQLCode = -1 THEN
		F_ErrorBaseDatos(sqlca,"Lectura de la Tabla fumigadet")
		RETURN True
	ELSEIF sqlca.SQLCode = 100 THEN
		RETURN True
	ELSE
		RETURN False
	END IF
	
Return False
end function

public function boolean wf_ValidaDatos ();Long ll_fila, ll_cont, ll_pallet
String	ls_mensaje
Integer	li_cliente, li_planta

li_cliente =	uo_SelCliente.Codigo
li_planta	=	uo_SelPlanta.Codigo

ls_mensaje = ''

FOR ll_fila = 1 TO dw_5.RowCount()
	
	ll_pallet = dw_5.Object.paen_numero[ll_fila]
	
	SELECT Count(*)
	INTO :ll_cont
	FROM dbo.palletfruta
	WHERE paen_numero = :ll_pallet
	AND clie_codigo = :li_cliente
	AND plde_codigo = :li_planta
	AND (isnull(PAFR_CUART1,0) = 0
	OR isnull(PAFR_CUART4,0) = 0
	OR isnull(PAFR_HUERT1,0) = 0
	OR isnull(PAFR_HUERT4,0) = 0);
	
	IF ll_cont > 0 THEN ls_mensaje = ls_mensaje + String(ll_Pallet)+','

NEXT	

IF ls_mensaje <> '' THEN
	MessageBox("Atención", "Pallet "+String(ls_mensaje)+" con Predio o Cuartel Nulo.~r~rArregle por Movimientos -> Mantención -> Mantención Predio Cuartel Detalle Pallet.", &
						Exclamation!, Ok!)
	Return True
END IF

Return False

	
end function

public function boolean wf_existeguia (long al_guia);Integer	li_tipoembq, li_consol, li_plades
Long		ll_fila, ll_fila1, ll_contpal, ll_cont

If al_guia <> 0 Then
	
	SELECT defe_tiposa,defe_numero,defe_especi,defe_consol,defe_plades, embq_codigo, defe_plasag
		INTO	:li_tipoembq,:il_despacho,:ii_especie,:li_consol,:li_plades, :is_Embarque, :al_planilla
		FROM	dbo.DESPAFRIGOEN 
		WHERE	plde_codigo =	:uo_SelPlanta.Codigo
		AND	clie_codigo	=	:uo_SelCliente.Codigo
		AND	defe_guides	=	:al_guia
		Using SQLCA;
				
	If sqlca.SQLCode = -1 Then
		F_ErrorBaseDatos(sqlca,"Lectura de la Tabla DESPAFRIGOEN")
		em_nroguia.SetFocus()
		Return False
	ElseIf sqlca.SQLCode = 100 Then
		MessageBox("Atención", "No existe Número Guia Despacho Indicado.~r~rIngrese otro Número.", Exclamation!, Ok!)
		pb_Grabar.Enabled	= False
		em_nroguia.Text 		= ''
		em_nroguia.SetFocus()
		Return False
	Else
		iuo_Embarque.Existe(is_Embarque, True, Sqlca)
		sle_nave.Text		= iuo_Embarque.NombreNave
		em_fzarpe.Text	= String(iuo_Embarque.FechaZarpe, 'dd/mm/yyyy')
		pb_Grabar.Enabled= True
	End If
Else
	MessageBox("Atención", "Faltan parámetros de búsqueda.~r~rIngreselos todos.", Exclamation!, Ok!)
	Return False
End If
end function

public function boolean wf_existearchivo (integer li_cliente, integer li_planta, long ll_guia, long al_planillas, string as_tipoplanilla, string as_instructivo);Date 		ld_fecha
Time 		lt_hora
Integer	li_contador, li_code_gengde
Long 		ll_numero

//ls_embarque = String(em_operacion.Text)

SELECT defe_numero
INTO	:ll_numero
FROM	dbo.DESPAFRIGOEN 
WHERE	plde_codigo =	:li_planta
AND	clie_codigo	=	:li_cliente
AND	defe_guides	=	:ll_guia
AND   IsNull(defe_plasag, -1) =  Case defe_tiposa when 33 Then -1 Else :al_planilla End
AND   embq_codigo =  :as_instructivo;

If sqlca.SQLCode = -1 Then
	F_ErrorBaseDatos(sqlca,"Lectura de la Tabla DESPAFRIGOEN")
	Return True
ElseIf sqlca.SQLCode = 100 Then
	MessageBox("Atención", "No existe Número Guia Indicado.~r~rIngrese otro Número.", Exclamation!, Ok!)
	Return True
End If

SELECT count(*),max(CODE_FECHAA),max(CODE_HORAAP)
INTO :li_contador,:ld_fecha,:lt_hora
FROM dbo.CONTROLDESPACHOS
where plde_codigo =	:li_planta
AND	clie_codigo	=	:li_cliente
AND	defe_numero	=	:ll_numero;

If sqlca.SQLCode = -1 Then
	F_ErrorBaseDatos(sqlca,"Lectura de la Tabla CONTROLDESPACHOS")
	Return True
End If

If li_contador > 0 Then
	Select CODE_GEMSAA 
	INTO :li_code_gengde
	from dbo.CONTROLDESPACHOS
	where  plde_codigo =	:li_planta
		AND	clie_codigo =	:li_cliente
		AND	defe_numero =	:ll_numero
		AND	CODE_FECHAA =  :ld_fecha
		AND	CODE_HORAAP = 	:lt_hora;
		
	If li_code_gengde = 1 Then
		MessageBox("Atención", "El Archivo S.A.A.M ya Fue Generado.", Exclamation!, Ok!)
		Return True
	End If		
End If

Return False
end function

on w_gene_archivo_saam.create
this.dw_6=create dw_6
this.dw_7=create dw_7
this.dw_4=create dw_4
this.dw_5=create dw_5
this.dw_guia=create dw_guia
this.cbx_resumen=create cbx_resumen
this.uo_selplanta=create uo_selplanta
this.uo_selcliente=create uo_selcliente
this.cbx_pak=create cbx_pak
this.st_7=create st_7
this.em_nroguia=create em_nroguia
this.cbx_prod=create cbx_prod
this.cbx_cal=create cbx_cal
this.cbx_var=create cbx_var
this.st_4=create st_4
this.st_3=create st_3
this.sle_mensa=create sle_mensa
this.em_fzarpe=create em_fzarpe
this.sle_nave=create sle_nave
this.st_5=create st_5
this.pb_salir=create pb_salir
this.pb_grabar=create pb_grabar
this.st_6=create st_6
this.st_1=create st_1
this.Control[]={this.dw_6,&
this.dw_7,&
this.dw_4,&
this.dw_5,&
this.dw_guia,&
this.cbx_resumen,&
this.uo_selplanta,&
this.uo_selcliente,&
this.cbx_pak,&
this.st_7,&
this.em_nroguia,&
this.cbx_prod,&
this.cbx_cal,&
this.cbx_var,&
this.st_4,&
this.st_3,&
this.sle_mensa,&
this.em_fzarpe,&
this.sle_nave,&
this.st_5,&
this.pb_salir,&
this.pb_grabar,&
this.st_6,&
this.st_1}
end on

on w_gene_archivo_saam.destroy
destroy(this.dw_6)
destroy(this.dw_7)
destroy(this.dw_4)
destroy(this.dw_5)
destroy(this.dw_guia)
destroy(this.cbx_resumen)
destroy(this.uo_selplanta)
destroy(this.uo_selcliente)
destroy(this.cbx_pak)
destroy(this.st_7)
destroy(this.em_nroguia)
destroy(this.cbx_prod)
destroy(this.cbx_cal)
destroy(this.cbx_var)
destroy(this.st_4)
destroy(this.st_3)
destroy(this.sle_mensa)
destroy(this.em_fzarpe)
destroy(this.sle_nave)
destroy(this.st_5)
destroy(this.pb_salir)
destroy(this.pb_grabar)
destroy(this.st_6)
destroy(this.st_1)
end on

event open;Boolean lb_Cerrar = False
x = 0
y = 0

This.Icon	=	Gstr_apl.Icono

If IsNull(uo_SelCliente.Codigo) Then lb_Cerrar = True
If IsNull(uo_SelPlanta.Codigo) Then lb_Cerrar = True

If lb_Cerrar Then
	Close(THis)
Else
	uo_SelCliente.Seleccion(False, False)
	uo_SelPlanta.Seleccion(False, False)
	uo_SelPlanta.Filtra(1)
	
	uo_SelCliente.Inicia(gi_codexport)
	uo_SelPlanta.Inicia(gi_codplanta)
	
	iuo_Despacho	=	Create uo_Despachos
	iuo_Puertos		=	Create uo_Puertos
	iuo_Embalaje	=	Create uo_Embalajes
	iuo_Envase		=	Create uo_Envases
	iuo_Embarque	=	Create uo_EmbarquesProd
	iuo_Guia			=	Create uo_GuiaDespacho
	
	is_report					=	'dw_info_guia_despacho_cal'
	dw_guia.DataObject	=	'dw_info_guia_despacho_cal'
	
	IF gi_vari_rotulada = 1 THEN
		cbx_var.Checked	=	True
		cbx_var.Enabled	=	False
	ELSE
		cbx_var.Checked	= 	False
		cbx_var.Enabled	=	True
	END IF	
	
	IF gi_prod_rotulado = 1 THEN
		cbx_prod.Checked	=	True
		cbx_prod.Enabled	=	False
	ELSE
		cbx_prod.Checked	= 	False
		cbx_prod.Enabled	=	True
	END IF
	
	IF gi_cali_rotulado = 1 THEN
		cbx_cal.Checked	=	True
		cbx_cal.Enabled	=	False
	ELSE
		cbx_cal.Checked	= 	False
		cbx_cal.Enabled	=	True
	END IF
	
	IF gi_pack_rotulado = 1 THEN
		cbx_pak.Checked	=	True
		cbx_pak.Enabled	=	False
	ELSE
		cbx_pak.Checked	= 	False
		cbx_pak.Enabled	=	True
	END IF
	
	ii_var			= gi_vari_rotulada
	ii_prod 		= gi_prod_rotulado
	ii_cal 			= gi_cali_rotulado
	ii_packing	= gi_pack_rotulado
	
	GrabaAccesoAplicacion(True, id_FechaAcceso, it_HoraAcceso, 	This.Title, "Acceso a Aplicación", 1)
End If
end event

type dw_6 from datawindow within w_gene_archivo_saam
boolean visible = false
integer x = 2386
integer y = 76
integer width = 192
integer height = 148
integer taborder = 50
boolean bringtotop = true
string title = "none"
string dataobject = "dw_info_archivo_saam_packing_nuevo"
end type

type dw_7 from datawindow within w_gene_archivo_saam
boolean visible = false
integer x = 2190
integer y = 76
integer width = 192
integer height = 148
integer taborder = 40
boolean bringtotop = true
string title = "none"
string dataobject = "dw_gene_archivo_saam_plano_nuevo"
end type

type dw_4 from datawindow within w_gene_archivo_saam
boolean visible = false
integer x = 2199
integer y = 240
integer width = 192
integer height = 148
integer taborder = 70
boolean bringtotop = true
string title = "none"
string dataobject = "dw_gene_archivo_saam_estado_nuevo_guia"
end type

type dw_5 from datawindow within w_gene_archivo_saam
boolean visible = false
integer x = 2409
integer y = 236
integer width = 192
integer height = 148
integer taborder = 70
boolean bringtotop = true
string title = "none"
string dataobject = "dw_controla_prediocuartel_guia"
end type

type dw_guia from uo_dw within w_gene_archivo_saam
boolean visible = false
integer x = 1966
integer y = 80
integer width = 206
integer height = 148
integer taborder = 30
boolean vscrollbar = false
end type

type cbx_resumen from checkbox within w_gene_archivo_saam
integer x = 1230
integer y = 712
integer width = 402
integer height = 80
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 16777215
long backcolor = 16711680
string text = "Resumen"
boolean checked = true
end type

type uo_selplanta from uo_seleccion_plantas within w_gene_archivo_saam
event destroy ( )
integer x = 626
integer y = 344
integer height = 100
integer taborder = 30
end type

on uo_selplanta.destroy
call uo_seleccion_plantas::destroy
end on

type uo_selcliente from uo_seleccion_clientesprod within w_gene_archivo_saam
event destroy ( )
integer x = 626
integer y = 220
integer height = 96
integer taborder = 30
end type

on uo_selcliente.destroy
call uo_seleccion_clientesprod::destroy
end on

type cbx_pak from checkbox within w_gene_archivo_saam
integer x = 727
integer y = 820
integer width = 530
integer height = 80
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Packing Rot."
end type

event clicked;if this.checked = true then
	ii_packing = 1
else
	ii_packing = 0
end if	
end event

type st_7 from statictext within w_gene_archivo_saam
integer x = 187
integer y = 488
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
string text = "Nro Guía"
boolean focusrectangle = false
end type

type em_nroguia from editmask within w_gene_archivo_saam
integer x = 635
integer y = 472
integer width = 347
integer height = 92
integer taborder = 50
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 16777215
alignment alignment = center!
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = stringmask!
string displaydata = "$"
end type

event modified;If Not wf_ExisteGuia(Long(This.Text)) Then
	This.SetFocus()
End If
end event

type cbx_prod from checkbox within w_gene_archivo_saam
integer x = 183
integer y = 820
integer width = 530
integer height = 80
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Productor Rot."
boolean checked = true
end type

event clicked;IF THIS.checked = true then
	ii_prod = 1
ELSE
	ii_prod = 0
END IF	
end event

type cbx_cal from checkbox within w_gene_archivo_saam
integer x = 727
integer y = 712
integer width = 439
integer height = 80
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Calidad Rot."
end type

event clicked;if this.checked = true then
	ii_cal = 1
else
	ii_cal = 0
end if	
end event

type cbx_var from checkbox within w_gene_archivo_saam
integer x = 183
integer y = 712
integer width = 480
integer height = 80
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Variedad Rot."
boolean checked = true
end type

event clicked;if this.checked = true then
	ii_var = 1
else
	ii_var = 0
end if	
end event

type st_4 from statictext within w_gene_archivo_saam
integer x = 178
integer y = 368
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

type st_3 from statictext within w_gene_archivo_saam
integer x = 178
integer y = 244
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

type sle_mensa from singlelineedit within w_gene_archivo_saam
integer x = 123
integer y = 988
integer width = 1769
integer height = 124
boolean bringtotop = true
integer textsize = -12
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = script!
string facename = "Comic Sans MS"
boolean italic = true
long textcolor = 65535
long backcolor = 553648127
boolean autohscroll = false
boolean displayonly = true
borderstyle borderstyle = stylelowered!
end type

type em_fzarpe from editmask within w_gene_archivo_saam
integer x = 1504
integer y = 592
integer width = 357
integer height = 92
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 16777215
long backcolor = 553648127
alignment alignment = center!
boolean displayonly = true
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = datemask!
string mask = "dd/mm/yyyy"
string displaydata = "@"
end type

type sle_nave from singlelineedit within w_gene_archivo_saam
integer x = 178
integer y = 592
integer width = 1285
integer height = 92
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 16777215
long backcolor = 553648127
boolean autohscroll = false
boolean displayonly = true
borderstyle borderstyle = stylelowered!
end type

type st_5 from statictext within w_gene_archivo_saam
integer x = 78
integer y = 68
integer width = 1851
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
string text = "Genera Archivo Plano Embarcador"
alignment alignment = center!
boolean border = true
borderstyle borderstyle = stylelowered!
boolean focusrectangle = false
end type

type pb_salir from picturebutton within w_gene_archivo_saam
integer x = 2071
integer y = 860
integer width = 343
integer height = 300
integer taborder = 70
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean cancel = true
string picturename = "\Desarrollo 17\Imagenes\Botones\Cancelar.png"
string disabledname = "\Desarrollo 17\Imagenes\Botones\Cancelar-bn.png"
alignment htextalign = left!
end type

event clicked;GrabaAccesoAplicacion(False, id_FechaAcceso, it_HoraAcceso, "", "", 0)
Close(Parent)
end event

type pb_grabar from picturebutton within w_gene_archivo_saam
integer x = 2066
integer y = 512
integer width = 343
integer height = 300
integer taborder = 60
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean enabled = false
boolean default = true
string picturename = "\Desarrollo 17\Imagenes\Botones\Guardar Cerrar.png"
string disabledname = "\Desarrollo 17\Imagenes\Botones\Guardar Cerrar-bn.png"
alignment htextalign = left!
end type

event clicked;SetPointer(Arrow!)

Integer	fila, li_tipoembq, li_vari, li_emba, li_calib, li_frio, li_marca, li_EmisionGuia
Long		ll_nroguia, ll_Fila, li_control = 0, ll_filpallet, ll_filetiqueta, ll_guia, ll_planilla, ll_filcont, ll_despacho
String		ls_DirectorioAct, ls_Archivo, ls_tipoIns,&
			ls_Ruta, ls_parteemb, ls_texto, ls_Instructivo, ls_embarque
			
If Not DirectoryExists(gs_disco+":\GeneradosSAAM") Then
	MessageBox('Error', 'El directorio para la generacion de archivo: ' + gs_disco+":\GeneradosSAAM" + &
					"~n~nNo existe, favor revisar o comunicarse con informatica, para activar directorio.", StopSign!, Ok!)
	Return
End If

OpenWithParm(vinf2, istr_info)

dw_guia.SetTransObject(sqlca)
ll_NroGuia	=	Long(em_nroguia.Text)

/*Selecciona tipo se salida del embarque*/
SELECT defe_tiposa, IsNull(defe_guiaem, 0), defe_numero
	INTO	:li_tipoembq, :li_EmisionGuia, :ll_despacho
	FROM	dbo.DESPAFRIGOEN 
	WHERE	plde_codigo =	:uo_SelPlanta.Codigo
	AND	clie_codigo	=	:uo_SelCliente.Codigo
	AND	defe_guides	=	:ll_nroguia;

If IsNull(uo_SelCliente.FormatoGuia) OR uo_SelCliente.FormatoGuia = '' Then
	vinf2.dw_1.DataObject = is_report
Else
	vinf2.dw_1.DataObject	= uo_SelCliente.FormatoGuia
	dw_guia.DataObject		= uo_SelCliente.FormatoGuia		
End If

vinf2.dw_1.SetTransObject(sqlca)

fila = vinf2.dw_1.Retrieve(uo_SelCliente.Codigo, uo_SelPlanta.Codigo, ll_nroguia,1,ii_var, ii_prod, ii_cal,li_vari,li_emba,li_calib,li_control, il_despacho)
fila = dw_guia.Retrieve(uo_SelCliente.Codigo, uo_SelPlanta.Codigo, ll_nroguia,1,ii_var, ii_prod, ii_cal,li_vari,li_emba,li_calib,li_control, il_despacho)

ls_DirectorioAct=GetCurrentDirectory()
ls_Archivo	=	ls_DirectorioAct + '\Guía Nº'+String(ll_nroguia)+' '+String(today(),'yyyymmdd')+'.pdf'

is_archivo = ls_Archivo
	
If fila = -1 Then
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
ElseIf fila = 0 Then
	MessageBox( "No Existe información", "No existe información para este informe.", StopSign!, Ok!)
Else
	F_Membrete(vinf2.dw_1)		
	If (li_tipoembq = 7 OR li_tipoembq = 8 OR li_tipoembq = 9 OR li_tipoembq = 33)  Then wf_GeneraArchivo()
	iuo_Guia.of_RecuperaPDF(ll_NroGuia, dw_guia.Object.defe_fecdes[1], 1)	
	dw_guia.SaveAs(gs_disco+":\GeneradosSAAM\"+'GD' +String(uo_SelCliente.Codigo,'000')+String(uo_SelPlanta.Codigo,'00000')+String(ll_nroguia, '00000000')+'.PDF', PDF!, TRUE)	
	pb_Grabar.Enabled = False
End If

SetPointer(Arrow!)	
end event

type st_6 from statictext within w_gene_archivo_saam
integer x = 82
integer y = 936
integer width = 1851
integer height = 224
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

type st_1 from statictext within w_gene_archivo_saam
integer x = 82
integer y = 184
integer width = 1851
integer height = 752
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


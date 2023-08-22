$PBExportHeader$w_maed_ctlcaldescarteespecies.srw
$PBExportComments$Ventana que permite el ingreso de fruta de descarte en packings por especies
forward
global type w_maed_ctlcaldescarteespecies from w_mant_encab_deta_csd
end type
end forward

global type w_maed_ctlcaldescarteespecies from w_mant_encab_deta_csd
integer width = 5010
integer height = 1908
string menuname = ""
boolean resizable = false
windowstate windowstate = maximized!
event ue_validapassword ( )
end type
global w_maed_ctlcaldescarteespecies w_maed_ctlcaldescarteespecies

type variables
DataWindowChild	idwc_zonas, idwc_plantas, idwc_inspectores, idwc_Productores, idwc_variedades, idwc_packings, idwc_categoria

uo_zonas              		iuo_Zonas
uo_plantadesp         		iuo_Plantas
uo_plantadesp         		iuo_Packing
uo_variedades         		iuo_Variedades
uo_Especie					iuo_Especie
uo_productores        		iuo_Productor
uo_ctlcalinspectores		iuo_ctlcalinspectores
uo_spro_ordenproceso	iuo_orden
uo_categoriaguarda		iuo_CateGuarda
uo_Cliente					iuo_Clientes
end variables

forward prototypes
protected function integer wf_modifica ()
public function boolean duplicado (string as_columna, string as_valor)
public subroutine habilitaingreso (string columna)
public function boolean noexistegrupo (string data)
public subroutine habilitaencab (boolean habilita)
public subroutine captura_usuario ()
public function boolean noexistelote (string as_valor)
public function boolean noexistelotes (integer ai_valor, integer ai_tipo)
public function boolean existefolio (string as_columna, string as_valor, integer ai_tipo)
public subroutine wf_buscalote (integer ai_tipo)
public subroutine wf_child ()
public subroutine wf_buscaorden ()
public function long wf_validadatos (long row, string columna, decimal valor)
end prototypes

event ue_validapassword();istr_mant.Argumento[1]	=	"Control de Calidad"
istr_mant.Argumento[2]	=	gstr_parlote.paswor

OpenWithParm(w_password, istr_mant)

istr_mant	=	Message.PowerObjectParm

IF istr_mant.Respuesta = 0 THEN Close(This)
end event

protected function integer wf_modifica ();IF dw_1.AcceptText() = -1 THEN RETURN -1
IF dw_2.AcceptText() = -1 THEN RETURN -1
IF (dw_1.ModifiedCount() + dw_1.DeletedCount()) > 0 THEN RETURN 0
IF dw_2.ModifiedCount() > 0 THEN RETURN 0
RETURN 1
end function

public function boolean duplicado (string as_columna, string as_valor);Long		ll_Fila,li_lote

CHOOSE CASE as_columna		
	CASE "lote_codigo"		
			li_lote = Integer(as_valor)
			
END CHOOSE	

ll_Fila	= dw_1.Find("lote_codigo =" + as_valor , 1, dw_1.RowCount())

IF ll_Fila > 0 AND ll_Fila <> il_Fila THEN
	MessageBox("Error","Número de Lote ya fue ingresado anteriormente",Information!, Ok!)
	RETURN True
ELSE
	RETURN False
END IF

end function

public subroutine habilitaingreso (string columna);Boolean	lb_estado = True

IF columna <> "clie_codigo" AND (dw_2.Object.clie_codigo[1] = 0 OR IsNull(dw_2.Object.clie_codigo[1])) THEN
	lb_estado = False
END IF

IF columna <> "cced_numero" AND (dw_2.Object.cced_numero[1] = 0 OR IsNull(dw_2.Object.cced_numero[1])) THEN
	lb_estado = False
END IF

IF columna <> "plde_codigo" AND (dw_2.Object.plde_codigo[1] = 0 OR IsNull(dw_2.Object.plde_codigo[1])) THEN
	lb_estado = False
END IF

IF columna <> "cced_tipore" AND (dw_2.Object.cced_tipore[1] = 0 OR IsNull(dw_2.Object.cced_tipore[1])) THEN
	lb_estado = False
END IF

IF columna <> "ccin_codigo" AND (dw_2.Object.ccin_codigo[1] = 0 OR IsNull(dw_2.Object.ccin_codigo[1])) THEN
	lb_estado = False
END IF

pb_ins_det.Enabled = lb_estado
pb_eli_det.Enabled = lb_estado
end subroutine

public function boolean noexistegrupo (string data);Integer li_Contador, li_Grupo

li_grupo = Integer(Data)

SELECT	Count(*)
	INTO 	:li_Contador
	FROM 	dbo.admagrupousuario
	WHERE	grpo_codigo = : li_grupo;

IF sqlca.sqlcode = -1 THEN
	F_ErrorBaseDatos(sqlca,"No se pudo leer la tabla Grupousuario")
	RETURN TRUE
ELSEIF li_Contador = 0 THEN
	RETURN TRUE
ELSE	
	istr_mant.argumento[1] = Data
	TriggerEvent("ue_recuperadatos")	
	RETURN FALSE	
END IF

RETURN FALSE
end function

public subroutine habilitaencab (boolean habilita);//IF Habilita THEN
//	dw_2.Object.ccte_numero.Protect				=	0
//	dw_2.Object.ccag_codigo.Protect				=	0
//	dw_2.Object.plde_codigo.Protect				=	0
//	dw_2.Object.ccte_fecins.Protect				=	0
//	dw_2.Object.zona_codigo.Protect				=	0
//	dw_2.Object.ccte_numero.BackGround.Color	=	RGB(255,255,255)
//	dw_2.Object.ccag_codigo.BackGround.Color	=	RGB(255,255,255)
//	dw_2.Object.plde_codigo.BackGround.Color	=	RGB(255,255,255)
//	dw_2.Object.ccte_fecins.BackGround.Color	=	RGB(255,255,255)
//	dw_2.Object.zona_codigo.BackGround.Color	=	RGB(255,255,255)
//	
//	pb_eliminar.Enabled	=	True
//
//ELSE
//	dw_2.Object.ccte_numero.Protect				=	1
//	dw_2.Object.ccag_codigo.Protect				=	1
//	dw_2.Object.plde_codigo.Protect				=	1
//	dw_2.Object.ccte_fecins.Protect				=	1
//	dw_2.Object.zona_codigo.Protect				=	1
//	dw_2.Object.ccte_numero.BackGround.Color	=	RGB(192,192,192)
//	dw_2.Object.ccag_codigo.BackGround.Color	=	RGB(192,192,192)
//	dw_2.Object.plde_codigo.BackGround.Color	=	RGB(192,192,192)
//	dw_2.Object.ccte_fecins.BackGround.Color	=	RGB(192,192,192)
//	dw_2.Object.zona_codigo.BackGround.Color	=	RGB(192,192,192)
//	
//	pb_eliminar.Enabled	=	False
//
//END IF
//
end subroutine

public subroutine captura_usuario ();dw_2.setitem(1,"usua_codigo",gstr_us.nombre)
dw_2.Setitem(1,"apac_fechaa",today())
dw_2.Setitem(1,"apac_horaac",now())
dw_2.Setitem(1,"comp_nombre",gstr_us.computador)
end subroutine

public function boolean noexistelote (string as_valor);Integer li_lote, li_existe, li_planta, li_especie, li_productor, li_variedad

li_lote   = Long(as_valor)
li_planta = Integer(istr_mant.argumento[4])

  SELECT	lot.lote_codigo,lot.lote_espcod,lot.lote_pltcod 
    INTO	:li_existe, :li_especie, :li_planta
    FROM	dbo.spro_lotesfrutagranel as lot   
    WHERE EXISTS(SELECT * FROM
      dbo.ctlcalrecepcionfrutasenca as rec WHERE
      lot.lote_pltcod = rec.plde_codigo AND
		lot.lote_espcod = rec.espe_codigo AND
		lot.lote_codigo = rec.lote_codigo AND
		lot.lote_codigo = :li_lote);

If sqlca.SQLCode = -1 Then
	F_errorbasedatos(sqlca,"Lectura tabla dbo.spro_lotesfrutagranel ")
	Return True
ElseIf li_existe > 0 Then
	dw_1.Object.lote_espcod[il_fila] = li_especie
	dw_1.Object.lote_pltcod[il_fila] = li_planta
	Return FALSE
Else
	MessageBox("Atención","Lote Digitado no existe, ingrese otro",exclamation!)
	Return True
END If
end function

public function boolean noexistelotes (integer ai_valor, integer ai_tipo);Integer li_lote, li_especie, li_Planta, li_Contador, li_variedad
Long ll_fila, ll_Planilla, ll_productor, ll_Cliente
String ls_Nula
SetNull(ls_Nula)

li_especie	=	iuo_Especie.Codigo
li_Planta		=	dw_2.Object.plde_codigo[1]
li_Lote		=	dw_1.Object.lote_codigo[1]
ll_productor = dw_1.Object.prod_codigo[1]//se agrego en join productor y variedad 
li_variedad 	=  dw_1.Object.vari_codigo[1]
ll_Cliente	= dw_2.Object.clie_codigo[1]

CHOOSE CASE ai_Tipo
	CASE 1
		iuo_Especie.Codigo	 =	ai_Valor
	CASE 2
		li_Planta	 =	ai_Valor
	CASE 3
		li_Lote		 =	ai_Valor
	CASE 4
		ll_productor =	ai_Valor
	CASE 5
		li_variedad	 =	ai_Valor
END CHOOSE

SELECT	Count(*)
	INTO	:li_Contador
	FROM	dbo.spro_lotesfrutagranel
	WHERE	lote_pltcod = :li_Planta
	AND   lote_espcod = :li_especie
	AND   lote_codigo = :li_lote
	AND   prod_codigo = :ll_productor
	AND   vari_codigo = :li_variedad;

IF sqlca.sqlcode = -1 THEN
	F_ErrorBaseDatos(sqlca,"No se pudo leer la tabla spro_lotesfrutagranel")
	RETURN TRUE
ELSEIF li_Contador = 0 THEN
	Messagebox("Atención","Lote No Existe en Fruta Granel")
	RETURN TRUE
ELSE
	
	SELECT	Count(*)
		INTO	:li_Contador
		FROM	dbo.ctlcalrecepcionfrutasenca
		WHERE	plde_codigo = :li_Planta
		AND   espe_codigo = :li_especie
		AND   lote_codigo = :li_lote
		AND 	clie_codigo = :ll_Cliente
		AND   prod_codigo = :ll_productor
		AND   vari_codigo = :li_variedad;
	
	IF sqlca.sqlcode = -1 THEN
		F_ErrorBaseDatos(sqlca,"No se pudo leer la tabla de Recepcióm")
		RETURN TRUE
	ELSEIF li_Contador = 0 THEN	
		Messagebox("Atención","Lote No Existe en Planilla de Recepción")
		RETURN TRUE	
	ELSE
		RETURN FALSE	
	END IF
END IF
end function

public function boolean existefolio (string as_columna, string as_valor, integer ai_tipo);Long		ll_nfolio, ll_existe, ll_Cliente

ll_Cliente = dw_2.Object.clie_codigo[1]
ll_nfolio = Long(as_valor)

  SELECT cced_numero 
    INTO :ll_existe  
    FROM dbo.ctlcalevaludescarteenca  
   WHERE clie_codigo = :ll_Cliente
	AND   cced_numero = :ll_nfolio
	And 	cced_tipore	= :ai_Tipo
	AND   espe_codigo = :iuo_Especie.Codigo ;// se incorpora especie en join 04/03/06

IF sqlca.SQLCode = -1 THEN
	F_errorbasedatos(sqlca,"Lectura tabla ctlcalevaludescarteenca  ")
	RETURN FALSE
ELSEIF ll_existe > 0 THEN
	istr_mant.argumento[1]	=	String(ll_nfolio)
	istr_mant.argumento[6]	=	String(ai_Tipo)
	This.TriggerEvent("ue_recuperadatos")				
	RETURN TRUE
ELSE
	RETURN FALSE
END IF

end function

public subroutine wf_buscalote (integer ai_tipo);Integer	 	li_Planta, li_Especie, li_zona, li_variedad
Long      		ll_fila, ll_productor, ll_Cliente

IF IsNull(dw_1.Object.clie_codigo[il_fila]) 	THEN
	ll_Cliente = -1
ELSE
	ll_Cliente = dw_1.Object.clie_codigo[il_fila]
END IF

IF IsNull(dw_1.Object.vari_codigo[il_fila]) 	THEN
	li_variedad = -1
ELSE
	li_variedad = dw_1.Object.vari_codigo[il_fila]
END IF
	
IF IsNull(dw_1.Object.prod_codigo[il_fila]) 	THEN
	ll_productor = -1
ELSE
	ll_productor = dw_1.Object.prod_codigo[il_fila]
END IF

istr_busq.argum[1] = String(iuo_Plantas.Codigo)
istr_busq.argum[2] = String(iuo_Especie.Codigo)
istr_busq.argum[6] =	String(li_variedad)
istr_busq.argum[7] =	String(ll_productor)

If ai_Tipo = 2 Then
	istr_busq.argum[4]=	String(dw_2.Object.zona_codigo[1])
	istr_busq.argum[5] =	'DESCARTA'
	istr_busq.argum[8] =	String(ll_Cliente)
Else
	istr_busq.argum[4] 	=	String(dw_1.Object.zona_codigo[il_Fila])
	istr_busq.argum[5] =	'Comercial'
End If

OpenWithParm(w_busc_lotefrutagranel, istr_busq)
	
istr_busq	= Message.PowerObjectParm
	
IF istr_busq.argum[3] <> "" THEN
	dw_1.Object.lote_codigo[il_fila]	=	Integer(istr_busq.argum[2])
	dw_1.Object.lote_pltcod[il_fila]		=	Integer(istr_busq.argum[3])
	dw_1.Object.lote_espcod[il_fila]	=	Integer(istr_busq.argum[14])
	dw_1.Object.prod_codigo[il_fila]	=	Long(istr_busq.argum[5])
	dw_1.Object.vari_codigo[il_fila]	=	Integer(istr_busq.argum[6])
	dw_1.Object.zona_codigo[il_fila]	=	Integer(istr_busq.argum[16])
	dw_1.Setcolumn("cced_precal")
	dw_1.SetFocus()
END IF







end subroutine

public subroutine wf_child ();dw_1.GetChild("zona_codigo", idwc_zonas)
idwc_zonas.SetTransObject(sqlca)
idwc_zonas.Retrieve()
idwc_zonas.SetSort("zona_nombre A")
idwc_zonas.Sort()

dw_2.GetChild("plde_codigo", idwc_plantas)
idwc_plantas.SetTransObject(sqlca)
idwc_plantas.Retrieve(1)
idwc_plantas.SetSort("plde_nombre A")
idwc_plantas.Sort()

dw_2.GetChild("plde_codpak", idwc_packings)
idwc_packings.SetTransObject(sqlca)
idwc_packings.Retrieve(2,0)
idwc_packings.SetSort("plde_nombre A")
idwc_packings.Sort()

dw_2.GetChild("ccin_codigo", idwc_inspectores)
idwc_inspectores.SetTransObject(sqlca)
If idwc_inspectores.Retrieve() = 0 Then
	idwc_inspectores.InsertRow(0)
End If

idwc_inspectores.SetSort("ccin_nombre A")
idwc_inspectores.Sort()
idwc_inspectores.InsertRow(0)

dw_1.GetChild("prod_codigo", idwc_productores)
idwc_productores.SetTransObject(sqlca)
If idwc_productores.Retrieve(-1) = 0 Then
	idwc_productores.InsertRow(0)
End If		
	
dw_1.GetChild("vari_codigo", idwc_variedades)
idwc_variedades.SetTransObject(sqlca)
If idwc_variedades.Retrieve(iuo_Especie.Codigo) = 0 Then
   idwc_variedades.InsertRow(0)
End If

dw_1.GetChild("ctcg_codigo", idwc_categoria)
idwc_categoria.SetTransObject(Sqlca)
If idwc_categoria.Retrieve(iuo_Especie.Codigo) = 0 Then
	idwc_categoria.InsertRow(0)
End If
end subroutine

public subroutine wf_buscaorden ();Integer	 	li_Planta, li_Especie, li_variedad, li_Tipo
Long      		ll_fila, ll_productor, ll_Cliente

If IsNull(dw_2.Object.clie_codigo[1]) Then
	ll_Cliente = -1
Else
	ll_Cliente = dw_2.Object.clie_codigo[1]
End If

IF IsNull(dw_1.Object.vari_codigo[il_fila]) 	THEN
	li_variedad = -1
ELSE
	li_variedad = dw_1.Object.vari_codigo[il_fila]
END IF

IF IsNull(dw_1.Object.orpr_tipord[il_fila]) 	THEN
	MessageBox('Atención', 'Debe seleccionar un tipo de orden primero.', StopSign!, OK!)
	Return
ELSE
	li_tipo = dw_1.Object.orpr_tipord[il_fila]
END IF
	
IF IsNull(dw_1.Object.prod_codigo[il_fila]) 	THEN
	ll_productor = -1
ELSE
	ll_productor = dw_1.Object.prod_codigo[il_fila]
END IF

istr_busq.argum[1] =	String(iuo_Plantas.Codigo)
istr_busq.argum[2] =	String(li_tipo)
istr_busq.argum[3] = String(iuo_Especie.Codigo)
istr_busq.argum[4] =	String(li_variedad)
istr_busq.argum[5] =	String(ll_productor)
istr_busq.argum[6] =	String(ll_Cliente)


OpenWithParm(w_busc_spro_ordenproceso, istr_busq)
	
istr_busq	= Message.PowerObjectParm
	
IF istr_busq.argum[3] <> "" THEN
	
	iuo_Productor.Existe(Long(istr_busq.argum[7]), False, Sqlca)
	
	dw_1.Object.orpr_numero[il_fila]	=	Long(istr_busq.argum[6])
	dw_1.Object.orpr_tipord[il_fila]	=	Integer(istr_busq.argum[10])
	dw_1.Object.lote_pltcod[il_fila]		=	Integer(istr_busq.argum[3])
	dw_1.Object.lote_espcod[il_fila]	=	Integer(istr_busq.argum[4])
	dw_1.Object.prod_codigo[il_fila]	=	Long(istr_busq.argum[7])
	dw_1.Object.vari_codigo[il_fila]	=	Integer(istr_busq.argum[8])
	dw_1.Object.zona_codigo[il_fila]	=	iuo_Productor.Zona
	dw_1.Setcolumn("cced_precal")
	dw_1.SetFocus()
END IF







end subroutine

public function long wf_validadatos (long row, string columna, decimal valor);Dec{2}	ld_Total,ld_Deform,ld_Defor2,ld_Defor3,ld_Defor4,ld_Golsol,ld_Hercit,ld_bajcol,ld_ausped,ld_mandor,ld_Mancha,ld_insect,ld_otrins,&
			ld_russet,ld_residu,ld_precal,ld_otrcal,ld_hersie,ld_medlun,ld_partid,ld_otrher,ld_pudric,ld_machuc,ld_frubla,ld_frudes,ld_desgar,ld_herabi,&
			ld_pittin,ld_deshid,ld_otrcon,ld_fruexp, ld_blandos,ld_piellagarto,ld_mancon,ld_danins,ld_carpar,ld_oleoce,ld_ombras,ld_parper,ld_peteca,ld_colvir,&
			ld_pateta,ld_ausros,ld_ombabi,ld_pedlar,ld_rugoso,ld_acosti,ld_bufado,ld_creasi, ld_fumagi, ld_danfri, ld_sobmad, ld_quesol,ld_grieta,ld_rusosc,ld_escama
			
If IsNull(Valor) Then Valor = 0
//ASIGNACION CAMPOS POR ESPECIE
Choose Case iuo_Especie.Codigo
	Case 21
		ld_Deform	= dw_1.Object.cced_deform[Row];		ld_Golsol		= dw_1.Object.cced_golsol[Row];		ld_Hercit		= dw_1.Object.cced_hercit[Row];
		ld_bajcol		= dw_1.Object.cced_bajcol[Row];		ld_ausped	= dw_1.Object.cced_ausped[Row];		ld_mandor	= dw_1.Object.cced_mandor[Row];
		ld_Mancha	= dw_1.Object.cced_mancha[Row];
		ld_precal		= dw_1.Object.cced_precal[Row];		ld_russet		= dw_1.Object.cced_russet[Row];		ld_residu		= dw_1.Object.cced_residu[Row];
		ld_otrcal		= dw_1.Object.cced_otrcal[Row];		ld_hersie		= dw_1.Object.cced_hersie[Row];		ld_medlun	= dw_1.Object.cced_medlun[Row];
		ld_partid		= dw_1.Object.cced_partid[Row];		ld_otrher		= dw_1.Object.cced_otrher[Row];		ld_fruexp	= dw_1.Object.cced_fruexp[Row];
		ld_otrcon	= dw_1.Object.cced_otrcon[Row];		ld_pudric		= dw_1.Object.cced_pudric[Row];		ld_deshid	= dw_1.Object.cced_deshid[Row];
		ld_frubla		= dw_1.Object.cced_frublan[Row];		ld_frudes	= dw_1.Object.cced_frudes[Row];		ld_desgar	= dw_1.Object.cced_desgar[Row];
		ld_pittin		= dw_1.Object.cced_pittin[Row];			ld_machuc	= dw_1.Object.cced_machuc[Row];		ld_mancon	= dw_1.Object.cced_mancon[Row];
		ld_piellagarto=dw_1.Object.cced_piella[Row];	
	Case 23 
		ld_bajcol		= dw_1.Object.cced_bajcol[Row];		ld_danins	= dw_1.Object.cced_danins[Row];		ld_Deform	= dw_1.Object.cced_deform[Row];
		ld_Golsol		= dw_1.Object.cced_golsol[Row];		ld_machuc	= dw_1.Object.cced_machuc[Row];		ld_pudric		= dw_1.Object.cced_pudric[Row];
		ld_otrcon	= dw_1.Object.cced_otrcon[Row];		ld_fruexp	= dw_1.Object.cced_fruexp[Row];		ld_herabi		= dw_1.Object.cced_herabi[Row];
		ld_Hercit		= dw_1.Object.cced_hercit[Row];		ld_insect		= dw_1.Object.cced_insect[Row];		ld_Mancha	= dw_1.Object.cced_mancha[Row];
		ld_residu		= dw_1.Object.cced_residu[Row];		ld_russet		= dw_1.Object.cced_russet[Row];		ld_otrcal		= dw_1.Object.cced_otrcal[Row];
		ld_frubla		= dw_1.Object.cced_frublan[Row];		ld_carpar	= dw_1.Object.cced_carpar[Row];		ld_frudes	= dw_1.Object.cced_frudes[Row];
		ld_desgar	= dw_1.Object.cced_desgar[Row];		ld_sobmad	= dw_1.Object.cced_sobmad[Row];
	Case 26, 27, 78, 36, 10
		ld_fumagi	= dw_1.Object.cced_fumagi[Row];		ld_machuc	= dw_1.Object.cced_machuc[Row];
		ld_oleoce	= dw_1.Object.cced_oleoce[Row];		ld_ombras	= dw_1.Object.cced_ombras[Row];		ld_parper	= dw_1.Object.cced_parper[Row];
		ld_pudric		= dw_1.Object.cced_pudric[Row];		ld_peteca	= dw_1.Object.cced_peteca[Row];		ld_pateta	= dw_1.Object.cced_pateta[Row];
		ld_deform	= dw_1.Object.cced_deform[Row];		ld_bajcol		= dw_1.Object.cced_bajcol[Row];		ld_ausros	= dw_1.Object.cced_ausros[Row];
		ld_hercit		= dw_1.Object.cced_hercit[Row];		ld_insect		= dw_1.Object.cced_insect[Row];		ld_Mancha	= dw_1.Object.cced_mancha[Row];
		ld_ombabi	= dw_1.Object.cced_ombabi[Row];		ld_pedlar		= dw_1.Object.cced_pedlar[Row];		ld_precal		= dw_1.Object.cced_precal[Row];
		ld_residu		= dw_1.Object.cced_residu[Row];		ld_rugoso	= dw_1.Object.cced_rugoso[Row];		ld_russet		= dw_1.Object.cced_russet[Row];
		ld_otrcal		= dw_1.Object.cced_otrcal[Row];		ld_acosti		= dw_1.Object.cced_acosti[Row];		ld_frubla		= dw_1.Object.cced_frublan[Row];
		ld_bufado	= dw_1.Object.cced_bufado[Row];		ld_creasi		= dw_1.Object.cced_creasi[Row];		ld_danfri		= dw_1.Object.cced_danfri[Row];
		ld_deshid	= dw_1.Object.cced_deshid[Row];		ld_herabi		= dw_1.Object.cced_herabi[Row];		ld_Golsol		= dw_1.Object.cced_golsol[Row];
		ld_otrcon	= dw_1.Object.cced_otrcon[Row];		ld_fruexp	= dw_1.Object.cced_fruexp[Row];
	Case 41
		ld_Deform	= dw_1.Object.cced_deform[Row];		ld_Defor2	= dw_1.Object.cced_defor2[Row];		ld_Defor3	= dw_1.Object.cced_defor3[Row];
		ld_Hercit		= dw_1.Object.cced_hercit[Row];		ld_insect		= dw_1.Object.cced_insect[Row];		ld_mandor	= dw_1.Object.cced_mandor[Row];
		ld_Mancha	= dw_1.Object.cced_mancha[Row];		ld_residu		= dw_1.Object.cced_residu[Row];		ld_russet		= dw_1.Object.cced_russet[Row];
		ld_otrcal		= dw_1.Object.cced_otrcal[Row];		ld_blandos	= dw_1.Object.cced_blandos[Row];		ld_Golsol		= dw_1.Object.cced_golsol[Row];
		ld_deshid	= dw_1.Object.cced_deshid[Row];		ld_herabi		= dw_1.Object.cced_herabi[Row];		ld_machuc	= dw_1.Object.cced_machuc[Row];
		ld_pudric		= dw_1.Object.cced_pudric[Row];		ld_frubla		= dw_1.Object.cced_frublan[Row];		ld_Defor4	= dw_1.Object.cced_defor4[Row];
		ld_otrcon	= dw_1.Object.cced_otrcon[Row];		ld_fruexp	= dw_1.Object.cced_fruexp[Row];		ld_Precal		= dw_1.Object.cced_precal[Row];
	Case 81
		ld_deform	= dw_1.Object.cced_deform[Row];		ld_fumagi	= dw_1.Object.cced_fumagi[Row];
		ld_hercit		= dw_1.Object.cced_hercit[Row];		ld_Mancha	= dw_1.Object.cced_mancha[Row];		ld_pedlar		= dw_1.Object.cced_pedlar[Row];
		ld_residu		= dw_1.Object.cced_residu[Row];		ld_russet		= dw_1.Object.cced_russet[Row];		ld_bajcol		= dw_1.Object.cced_bajcol[Row];
		ld_colvir		= dw_1.Object.cced_colvir[Row];			ld_escama	= dw_1.Object.cced_escama[Row];		ld_insect		= dw_1.Object.cced_insect[Row];
		ld_otrins		= dw_1.Object.cced_otrins[Row];		ld_otrcal		= dw_1.Object.cced_otrcal[Row];		ld_ausped	= dw_1.Object.cced_ausped[Row];
		ld_frubla		= dw_1.Object.cced_frublan[Row];		ld_herabi		= dw_1.Object.cced_herabi[Row];		ld_machuc	= dw_1.Object.cced_machuc[Row];
		ld_pudric		= dw_1.Object.cced_pudric[Row];		ld_quesol	= dw_1.Object.cced_quesol[Row];		ld_otrcon	= dw_1.Object.cced_otrcon[Row];
		ld_fruexp	= dw_1.Object.cced_fruexp[Row];		ld_danfri		= dw_1.Object.cced_danfri[Row];		ld_desgar	= dw_1.Object.cced_desgar[Row];
		ld_deshid	= dw_1.Object.cced_deshid[Row];		ld_Golsol		= dw_1.Object.cced_golsol[Row];			
	Case 82
		ld_russet		= dw_1.Object.cced_russet[Row];		ld_residu		= dw_1.Object.cced_residu[Row];		ld_precal		= dw_1.Object.cced_precal[Row];
		ld_otrcal		= dw_1.Object.cced_otrcal[Row];		ld_deshid	= dw_1.Object.cced_deshid[Row];		ld_Golsol		= dw_1.Object.cced_golsol[Row];
		ld_herabi		= dw_1.Object.cced_herabi[Row];		ld_quesol	= dw_1.Object.cced_quesol[Row];		ld_machuc	= dw_1.Object.cced_machuc[Row];
		ld_pudric		= dw_1.Object.cced_pudric[Row];		ld_otrcon	= dw_1.Object.cced_otrcon[Row];		ld_fruexp	= dw_1.Object.cced_fruexp[Row];
		ld_Deform	= dw_1.Object.cced_deform[Row];		ld_grieta		= dw_1.Object.cced_grieta[Row];		ld_Hercit		= dw_1.Object.cced_hercit[Row];
		ld_insect		= dw_1.Object.cced_insect[Row];		ld_Mancha	= dw_1.Object.cced_mancha[Row];		ld_rusosc	= dw_1.Object.cced_rusosc[Row];
		ld_bajcol		= dw_1.Object.cced_bajcol[Row];		
End Choose

//VERIFICACION CAMPOS TODAS LAS ESPECIES
If IsNull(ld_Deform) Then ld_Deform=0;	If IsNull(ld_Defor2)Then	ld_Defor2=0;	If IsNull(ld_Defor3) Then ld_Defor3=0;	If IsNull(ld_Golsol) Then	ld_Golsol=0;
If IsNull(ld_Hercit) Then	ld_Hercit=0;	If IsNull(ld_mandor) Then ld_mandor=0;	If IsNull(ld_Mancha) Then ld_Mancha=0;If IsNull(ld_insect) Then ld_insect=0;
If IsNull(ld_russet) Then ld_russet=0;	If IsNull(ld_residu) Then	ld_residu=0;	If IsNull(ld_otrcal) Then	ld_otrcal=0;		If IsNull(ld_pudric) Then	ld_pudric=0;
If IsNull(ld_machuc) Then ld_machuc=0;	If IsNull(ld_frubla) Then ld_frubla=0;		If IsNull(ld_herabi) Then	ld_herabi=0;	If IsNull(ld_deshid) Then	ld_deshid=0;
If IsNull(ld_otrcon) Then	ld_otrcon=0;	If IsNull(ld_fruexp) Then	ld_fruexp=0;	If IsNull(ld_blandos) Then ld_blandos=0;If IsNull(ld_piellagarto) Then ld_piellagarto=0;
If IsNull(ld_bajcol) Then ld_bajcol=0;		If IsNull(ld_ausped) Then ld_ausped=0;	If IsNull(ld_colvir) Then ld_colvir=0;		If IsNull(ld_escama) Then ld_escama=0;
If IsNull(ld_precal) Then ld_precal=0;		If IsNull(ld_hersie) Then ld_hersie=0;		If IsNull(ld_medlun) Then ld_medlun=0;	If IsNull(ld_partid) Then ld_partid=0;
If IsNull(ld_otrher) Then ld_otrher=0;	If IsNull(ld_frudes) Then ld_frudes=0;	If IsNull(ld_desgar) Then ld_desgar=0;	If IsNull(ld_herabi) Then ld_herabi=0;
If IsNull(ld_pittin) Then ld_pittin=0;		If IsNull(ld_mancon) Then ld_mancon=0;If IsNull(ld_danins) Then ld_danins=0;	If IsNull(ld_carpar) Then ld_carpar=0;
If IsNull(ld_oleoce) Then ld_oleoce=0;	If IsNull(ld_ombras) Then ld_ombras=0;	If IsNull(ld_parper) Then ld_parper=0;	If IsNull(ld_peteca) Then ld_peteca=0;
If IsNull(ld_pateta) Then ld_pateta=0;	If IsNull(ld_ausros) Then ld_ausros=0;	If IsNull(ld_ombabi) Then ld_ombabi=0;	If IsNull(ld_pedlar) Then ld_pedlar=0;
If IsNull(ld_rugoso) Then ld_rugoso=0;	If IsNull(ld_acosti) Then ld_acosti=0;		If IsNull(ld_bufado) Then ld_bufado=0;	If IsNull(ld_creasi) Then ld_creasi=0;
If IsNull(ld_quesol) Then ld_quesol=0;	If IsNull(ld_rusosc) Then ld_rusosc=0;	If IsNull(ld_grieta) Then ld_grieta=0;		If IsNull(ld_otrins) Then ld_otrins=0;
If IsNull(ld_defor4) Then ld_defor4=0;
//ASIGNACION VALOR COLUMNA ESPECIFICA
Choose Case Columna	
	Case 'cced_deform'
		ld_Deform = Valor
	Case 'cced_defor2'
		ld_Defor2 = Valor
	Case 'cced_defor3'
		ld_Defor3 = Valor
	Case 'cced_defor4'
		ld_Defor4 = Valor
	Case 'cced_golsol'
		ld_Golsol = Valor
	Case 'cced_hercit'
		ld_Hercit = Valor
	Case 'cced_mandor'
		ld_mandor = Valor
	Case 'cced_mancha'
		ld_Mancha = Valor
	Case 'cced_insect'
		ld_insect = Valor
	Case 'cced_russet'
		ld_russet = Valor
	Case 'cced_residu'
		ld_residu = Valor
	Case 'cced_otrcal'
		ld_otrcal = Valor
	Case 'cced_pudric'
		ld_pudric = Valor
	Case 'cced_machuc'
		ld_machuc = Valor
	Case 'cced_frublan'
		ld_frubla = Valor
	Case 'cced_herabi'
		ld_herabi = Valor
	Case 'cced_deshid'
		ld_deshid = Valor
	Case 'cced_otrcon'
		ld_otrcon = Valor
	Case 'cced_fruexp'
		ld_fruexp = Valor
	Case 'cced_blandos'
		ld_blandos = Valor
	Case 'cced_piella'
		ld_piellagarto = Valor
	Case 'cced_bajcol'
		ld_bajcol = Valor
	Case 'cced_ausped'
		ld_ausped = Valor
	Case 'cced_precal'
		ld_precal = Valor
	Case 'cced_hersie'
		ld_hersie = Valor
	Case 'cced_medlun'
		ld_medlun = Valor
	Case 'cced_partid'
		ld_partid = Valor
	Case 'cced_otrher'
		ld_otrher = Valor
	Case 'cced_frudes'
		ld_frudes = Valor
	Case 'cced_desgar'
		ld_desgar = Valor
	Case 'cced_herabi'
		ld_herabi = Valor
	Case 'cced_pittin'
		ld_pittin = Valor
	Case 'cced_mancon'
		ld_mancon = Valor
	Case 'cced_danins'
		ld_danins = Valor
	Case 'cced_carpar'
		ld_carpar = Valor
	Case 'cced_oleoce'
		ld_oleoce = Valor
	Case 'cced_ombras'
		ld_ombras = Valor
	Case 'cced_parper'
		ld_parper = Valor
	Case 'cced_peteca'
		ld_peteca = Valor
	Case 'cced_pateta'
		ld_pateta = Valor
	Case 'cced_ausros'
		ld_ausros = Valor
	Case 'cced_ombabi'
		ld_ombabi = Valor
	Case 'cced_pedlar'
		ld_pedlar = Valor
	Case 'cced_rugoso'
		ld_rugoso = Valor
	Case 'cced_acosti'
		ld_acosti = Valor
	Case 'cced_bufado'
		ld_bufado = Valor
	Case 'cced_creasi'
		ld_creasi = Valor
	Case 'cced_fumagi'
		ld_fumagi = Valor
	Case 'cced_danfri'
		ld_danfri = Valor
	Case 'cced_sobmad'
		ld_sobmad = Valor
	Case 'cced_grieta'  
		ld_grieta = Valor
	Case 'cced_rusosc'
		ld_rusosc =Valor
	Case 'cced_quesol'
		ld_quesol = Valor
	Case 'cced_otrins'
		ld_otrins = Valor
	Case 'cced_colvir'
		ld_colvir = Valor
	Case 'cced_escama'
		ld_escama = Valor
End Choose

//SUMATORIA DE TODOS LOS DEFECTOS
ld_Total =	ld_Deform + ld_Defor2 + ld_Defor3 + ld_Golsol + ld_Hercit + ld_mandor + ld_Mancha + ld_insect + ld_russet + ld_residu + ld_otrcal + ld_pudric + &
				ld_machuc + ld_frubla + ld_herabi + ld_deshid + ld_otrcon + ld_fruexp + ld_blandos + ld_piellagarto + ld_bajcol + ld_ausped + &
				ld_precal + ld_hersie + ld_medlun + ld_partid + ld_otrher + ld_frudes + ld_desgar + ld_pittin + ld_mancon + ld_danins + & 
				ld_carpar + ld_oleoce + ld_ombras + ld_parper + ld_peteca + ld_pateta + ld_ausros + ld_ombabi + ld_pedlar + ld_rugoso + ld_acosti + ld_bufado + &
				ld_creasi + ld_fumagi + ld_danfri + ld_sobmad + ld_quesol + ld_grieta + ld_rusosc + ld_otrins + ld_colvir + ld_escama + ld_defor4
				
If ld_Total > 100 Then Return -1

Return 0
end function

on w_maed_ctlcaldescarteespecies.create
call super::create
end on

on w_maed_ctlcaldescarteespecies.destroy
call super::destroy
end on

event ue_seleccion;If dw_2.Object.plde_codigo[1] = 0 OR IsNull(dw_2.Object.plde_codigo[1]) Then
	MessageBox("Atención","Debe Seleccionar Planta",exclamation!)
	dw_2.SetColumn("plde_codigo")
	dw_2.SetFocus()
	Return 
ElseIf dw_2.Object.clie_codigo[1] = 0 OR IsNull(dw_2.Object.clie_codigo[1]) Then
	MessageBox("Atención","Debe Seleccionar Cliente",exclamation!)
	dw_2.SetColumn("clie_codigo")
	dw_2.SetFocus()
	Return 
Else
	istr_busq.argum[1]	=	String(dw_2.Object.clie_codigo[1])
	istr_busq.argum[2]	=  String(dw_2.Object.plde_codigo[1])
	istr_busq.argum[3]	=  String(iuo_Especie.Codigo)
	istr_busq.argum[4] 	=	String(dw_2.Object.cced_tipore[1])
	
	OpenWithParm(w_busc_descartafruta, istr_busq)
	
	istr_busq = Message.PowerObjectParm
	
	IF istr_busq.argum[4] <> "" THEN
		istr_mant.argumento[1] = istr_busq.argum[1]
		istr_mant.argumento[5] = istr_busq.argum[4]
		dw_2.Object.clie_codigo[1] = Long(istr_busq.argum[4])
		istr_mant.argumento[6] = istr_busq.argum[5]
		This.TriggerEvent("ue_recuperadatos")
	ELSE
		pb_buscar.SetFocus()
	END IF
END IF


end event

event ue_recuperadatos;Long 		ll_fila_e, ll_fila_d, ll_fila_f, respuesta

DO
	dw_2.SetRedraw(False)
	dw_2.Reset()
	
	ll_fila_e	=	dw_2.Retrieve(Integer(istr_mant.argumento[5]), Long(istr_mant.argumento[1]),iuo_Especie.Codigo, Integer(istr_mant.argumento[6]))
	
	IF ll_fila_e = -1 THEN
		respuesta = MessageBox("Error en Base de Datos", "No es posible conectar la Base de Datos.", Information!, RetryCancel!)
	ELSE
		DO			
   		ll_fila_d	= dw_1.Retrieve(Integer(istr_mant.argumento[5]), Long(istr_mant.argumento[1]),iuo_Especie.Codigo, Integer(istr_mant.argumento[6]))

		IF ll_fila_d = -1 THEN				
			respuesta = MessageBox("Error en Base de Datos", "No es posible conectar la Base de Datos.", Information!, RetryCancel!)
		ELSE
			pb_grabar.Enabled		= True
			pb_ins_det.Enabled	= True
		END IF	
			
		wf_child()	
		
		IF ll_fila_d > 0 THEN	
			pb_imprimir.Enabled	= True
			pb_eliminar.Enabled	= True
			dw_1.SetRow(1)
			dw_1.SelectRow(1,False)
			dw_1.SetFocus()				
		ELSE
			pb_ins_det.SetFocus()				
		END IF
			
		LOOP WHILE respuesta = 1
		
		HabilitaEncab(False)

		IF respuesta = 2 THEN Close(This)
	END IF
	dw_2.SetRedraw(True)
LOOP WHILE respuesta = 1
dw_2.groupCalc()
IF respuesta = 2 THEN Close(This)
end event

event ue_nuevo;Long		ll_modif1, ll_modif2, ll_modif3
Integer	li_Grupo

ib_ok	= True

istr_busq.argum[1]		=	""
istr_busq.argum[2]		=	""
istr_busq.argum[4]		=	""
istr_busq.argum[5]		=	""

istr_Mant.Argumento[1]	=	""
istr_Mant.Argumento[3]	=	""
istr_mant.argumento[6]	=	""


istr_mant.Solo_Consulta	=	istr_mant.UsuarioSoloConsulta

If Not istr_mant.Solo_Consulta Then
	Choose Case wf_modifica()			
		Case -1
			ib_ok = False			
		Case 0
			ll_modif1	=	dw_1.GetNextModified(0, Primary!)
			ll_modif2	=	dw_2.GetNextModified(0, Primary!)	

			If dw_1.RowCount() > 0 Then
				Choose Case MessageBox("Grabar registro(s)","Desea Grabar la información ?", Question!, YesNoCancel!)
					Case 1
						Message.DoubleParm = 0
						This.TriggerEvent("ue_guardar")
						IF message.DoubleParm = -1 THEN ib_ok = False
					Case 3
						ib_ok	= False
						RETURN
				End Choose 
			End If
	End Choose
End If

If Not ib_ok Then Return

dw_1.Reset()

HabilitaEncab(True)

pb_eliminar.Enabled	=	False
pb_eli_det.Enabled		=	False
pb_ins_det.Enabled	=	False
pb_grabar.Enabled		=	False
pb_imprimir.Enabled	=	False
dw_2.Enabled			=	True

dw_2.SetRedraw(False)
dw_2.Reset()
dw_2.InsertRow(0)
dw_2.SetRedraw(True)

li_Grupo = BuscaGrupo(Upper(Gstr_Us.Nombre))

If (li_Grupo > 2) Then
	pb_eliminar.Visible	=	False
	TriggerEvent('resize')
End If

dw_2.Object.cced_tipore[1] 	= 2
dw_2.Object.clie_codigo[1]		=	gi_codexport
dw_2.Object.cced_fecmov[1]	=	Today()
dw_2.Object.espe_codigo[1]	=	iuo_Especie.Codigo

dw_2.SetColumn("clie_codigo")

Choose Case iuo_Especie.Codigo
	Case 21
		dw_1.DataObject = 'dw_mues_ctlcaldescarteespecies'
	Case 41
		dw_1.DataObject = 'dw_mues_ctlcaldescartekiwis'
	Case 26, 27, 78, 36, 10
		dw_1.DataObject = 'dw_mues_ctlcaldescartenaranjas'
	Case 81
		dw_1.DataObject = 'dw_mues_ctlcaldescartepaltas'
	Case 82
		dw_1.DataObject = 'dw_mues_ctlcaldescartegranados'
	Case 23
		dw_1.DataObject = 'dw_mues_ctlcaldescarteciruela'
End Choose

dw_1.SetTransObject(Sqlca)

wf_Child()
end event

event ue_nuevo_detalle;call super::ue_nuevo_detalle;If il_fila > 0 THEN
	pb_eliminar.Enabled	= True
	pb_grabar.Enabled	= True
End If

If dw_1.RowCount()	>	0	THEN	
	dw_1.GetChild("vari_codigo", idwc_variedades)
   	idwc_variedades.SetTransObject(sqlca)
   	If idwc_variedades.Retrieve(iuo_Especie.Codigo) = 0 THEN
      	idwc_variedades.InsertRow(0)
	End If
	
	If IsNull(dw_1.Object.prod_codigo[il_fila]) OR dw_1.Object.vari_codigo[il_fila] = 0  OR  dw_1.Object.lote_codigo[il_fila] = 0 THEN 
			MessageBox("Atención","Falta Ingresar Productor, Variedad o Lote, Verifique",Exclamation!)
			dw_1.SetColumn("zona_codigo")
	ElseIf dw_1.Object.total_frutos[il_fila] <> 100 THEN
		Messagebox("Atención","Total Frutos debe ser igual 100 % , VerIfique", Exclamation!) 		
	Else
		il_fila = dw_1.InsertRow(0)
		dw_1.Setfocus()
		dw_1.ScrollToRow(il_fila)
		dw_1.SetRow(il_fila)
	  	dw_1.SetColumn("zona_codigo")		
	End If	
Else	
	If dw_1.RowCount()	=	0	THEN il_fila = dw_1.InsertRow(0)	
	
	dw_1.Setfocus()
	dw_1.ScrollToRow(il_fila)
	dw_1.SetRow(il_fila)
	dw_1.SetColumn("zona_codigo")		
End If
end event

event open;/*Argumentos Utilizados 

Argumento[1]	=	Folio
Argumento[2]	=	Zona
Argumento[3]	=	Packing
Argumento[4]	=  Planta 
Argumento[5]	=	Cliente
Argumento[6]	=	Fecha de Proceso
Argumento[7]	=	Especie
Argumento[8]	=	Productor
Argumento[9]	=	Variedad
*/
x				= 0
y				= 0

This.Height	= 2520
im_menu		= m_principal

This.Icon										=	Gstr_apl.Icono
This.ParentWindow().ToolBarVisible	=	True
im_menu.Item[1].Item[6].Enabled		=	True
im_menu.Item[7].Visible					=	True

iuo_Especie					=	Create	uo_Especie
iuo_zonas       	   			= 	Create	uo_zonas
iuo_plantas     	   			= 	Create	uo_plantadesp
iuo_packing					=	Create 	uo_plantadesp 
iuo_variedades  	   		= 	Create	uo_variedades 
iuo_productor   	   		= 	Create	uo_productores
iuo_ctlcalinspectores   	= 	Create	uo_ctlcalinspectores
iuo_orden					=	Create 	uo_spro_ordenproceso
iuo_CateGuarda			=	Create	uo_categoriaguarda
iuo_Clientes					=	Create 	uo_Cliente

iuo_Especie.Existe(Integer(Message.StringParm), False, Sqlca)
iuo_Plantas.Existe(gi_CodPlanta, False, Sqlca)

This.Title	= "EVALUACIÓN FRUTA COMERCIAL DE : " + Upper(iuo_Especie.Nombre)

dw_1.SetTransObject(sqlca)
dw_2.SetTransObject(sqlca)

dw_1.ModIfy("datawindow.message.title='Error '+ is_titulo")
dw_1.SetRowFocusIndicator(Hand!)
dw_1.ModIfy("DataWindow.Footer.Height = 110")

istr_mant.dw						=	dw_1
istr_mant.UsuarioSoloConsulta	=	OpcionSoloConsulta()

pb_nuevo.PostEvent(Clicked!)
dw_2.Object.espe_codigo[1] = iuo_Especie.Codigo
wf_child()

istr_mant.argumento[5]	=	String(gi_CodExport)
istr_mant.argumento[6]	=	String(Today())
istr_mant.argumento[7]	=	String(iuo_Especie.Codigo)
istr_mant.argumento[4]	=	String(gi_CodPlanta)
istr_mant.Argumento[8]	=	String(gi_CodProductor)
istr_mant.Argumento[9]	=	String(gi_CodVariedad)

GrabaAccesoAplicacion(True, id_FechaAcceso, it_HoraAcceso, This.Title, "Acceso a Aplicación", 1)
end event

event ue_borra_detalle;IF dw_1.rowcount() < 1 THEN RETURN

SetPointer(HourGlass!)

ib_borrar = True
w_main.SetMicroHelp("Validando la eliminación de detalle...")

Message.DoubleParm = 0

This.TriggerEvent ("ue_validaborrar_detalle")

IF Message.DoubleParm = -1 THEN RETURN

IF dw_1.GetItemStatus(il_Fila,0,Primary!)	=	NewModified!	THEN
	IF	dw_1.GetItemStatus(il_Fila,0,Primary!)	=	NewModified!	THEN	
		IF MessageBox("Eliminación de Registro", "Está seguro de Eliminar este Registro", Question!, YesNo!) = 1 THEN
			IF dw_1.DeleteRow(0) = 1 THEN
				ib_borrar = False
				w_main.SetMicroHelp("Borrando Registro...")
				SetPointer(Arrow!)
			ELSE
				ib_borrar = False
				MessageBox(This.Title,"No se puede borrar actual registro.")
			END IF
		 IF dw_1.RowCount() = 0 THEN
				pb_eliminar.Enabled = False
			ELSE
				il_fila = dw_1.GetRow()
			END IF
		END IF
	ELSE
		MessageBox("Atención","No se borrarán registros ya almacenados")
	END IF
ELSE
	MessageBox("Atención","No se borrarán registros ya almacenados")
END If
end event

event ue_imprimir;SetPointer(HourGlass!)

Long		fila

istr_info.titulo	=	"INFORME DE EVALUACIÓN FRUTA COMERCIAL"
istr_info.copias	=	1

OpenWithParm(vinf,istr_info)

If iuo_Especie.Codigo = 21 Then
	If dw_2.Object.cced_tipore[1] = 1 Then
		vinf.dw_1.DataObject	=	"dw_info_consultaevaluadescartefruta_huerto"
		vinf.dw_1.Object.DataWindow.Zoom = 75
	Else
		vinf.dw_1.DataObject	=	"dw_info_consultaevaluadescartefruta"
		vinf.dw_1.Object.DataWindow.Zoom = 88
	End If
ElseIf iuo_Especie.Codigo = 41 Then
	vinf.dw_1.DataObject	=	"dw_info_ctlcalevaluadescartekiwis"
	vinf.dw_1.Object.DataWindow.Zoom = 85
ElseIf iuo_Especie.Codigo = 27 Or iuo_Especie.Codigo = 26 Or iuo_Especie.Codigo = 78 or iuo_Especie.Codigo = 36 Or iuo_Especie.Codigo = 10 Then
	If dw_2.Object.cced_tipore[1] = 1 Then
		vinf.dw_1.DataObject	=	"dw_info_ctlcalevaluedescartenara_Huerto"
		vinf.dw_1.Object.DataWindow.Zoom = 78
	Else
		vinf.dw_1.DataObject	=	"dw_info_ctlcalevaluedescartenara"
		vinf.dw_1.Object.DataWindow.Zoom = 70
	End If
ElseIf iuo_Especie.Codigo = 81 Then
	If dw_2.Object.cced_tipore[1] = 1 Then
		vinf.dw_1.DataObject	=	"dw_info_ctlcalevaluedescartepaltas_huerto"
		vinf.dw_1.Object.DataWindow.Zoom = 95
	Else
		vinf.dw_1.DataObject	=	"dw_info_ctlcalevaluedescartepaltas"
		vinf.dw_1.Object.DataWindow.Zoom = 87
	End If
ElseIf iuo_Especie.Codigo = 82 Then
	vinf.dw_1.DataObject	=	"dw_info_descarte_granadas"
	vinf.dw_1.Object.DataWindow.Zoom = 80
ElseIf iuo_Especie.Codigo = 23 Then
	vinf.dw_1.DataObject	=	"dw_info_descarte_ciruela"
	vinf.dw_1.Object.DataWindow.Zoom = 65
End If
vinf.dw_1.SetTransObject(sqlca)

If iuo_Especie.Codigo = 23 Or iuo_Especie.Codigo = 82 Then
	fila	=	vinf.dw_1.Retrieve(Integer(istr_Mant.Argumento[5]),iuo_Especie.Codigo, dw_2.Object.cced_numero[1], dw_2.Object.cced_tipore[1], 1, &
								-1, -1, -1, Date('19000101'), Today())
ElseIf iuo_Especie.Codigo = 21 Then
	fila	=	vinf.dw_1.Retrieve(Integer(istr_Mant.Argumento[5]),iuo_Especie.Codigo, -1, -1, -1, -1, Date('19000101'), Today(), 0, dw_2.Object.cced_tipore[1], dw_2.Object.cced_numero[1])	
Else
	fila	=	vinf.dw_1.Retrieve(Integer(istr_Mant.Argumento[5]),iuo_Especie.Codigo, dw_2.Object.cced_numero[1], dw_2.Object.cced_tipore[1])
End If

If fila	=	-1 Then
	MessageBox( "ErrOr en Base de Datos", "Se ha producido un errOr en Base " + &
					"de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
ElseIf fila	=	0 Then
	MessageBox( "No Existe información", "No existe información para este informe.", &
					StopSign!, Ok!)
Else
	F_Membrete(vinf.dw_1)
	IF gs_Ambiente <> 'Windows' THEN
		F_ImprimeInformePdf(vinf.dw_1, istr_info.titulo)

	END IF
End If

SetPointer(Arrow!)
end event

event ue_antesguardar;Long		ll_Numero, ll_fila , ll_fildet
Integer	li_Cliente, li_Secuen, li_cont, li_Causal, li_cont1, li_secuencia, li_lote, li_planta, li_Tipo
String		ls_Mensaje, ls_colu[]

If dw_2.RowCount() > 0 Then
		
	If Isnull(dw_2.Object.cced_numero[1]) OR dw_2.Object.cced_numero[1] = 0 Then
		li_cont ++
		ls_mensaje 			= ls_mensaje + "~Nº de Planilla"
		ls_colu[li_cont]	= "cced_numero"
	End If
	
	If Isnull(dw_2.Object.plde_codigo[1]) OR dw_2.Object.plde_codigo[1] = 0 Then
		li_cont ++
		ls_mensaje 			= ls_mensaje + "~nPlanta"
		ls_colu[li_cont]	= "plde_codigo"
	End If
	
	If Isnull(dw_2.Object.plde_codpak[1]) OR dw_2.Object.plde_codpak[1] = 0 Then
		li_cont ++
		ls_mensaje 			= ls_mensaje + "~nPacking"
		ls_colu[li_cont]	= "plde_codpak"
	End If
	
	If Isnull(dw_2.Object.ccin_codigo[1]) OR dw_2.Object.ccin_codigo[1] = 0 Then
		li_cont ++
		ls_mensaje 			= ls_mensaje + "~nInspector"
		ls_colu[li_cont]	= "ccin_codigo"
	End If
	
	For ll_fildet = 1 to dw_1.RowCount()

		If Isnull(dw_1.Object.zona_codigo[ll_fildet]) OR dw_1.Object.zona_codigo[ll_fildet] = 0 Then
			li_cont1 ++
			ls_mensaje 			= ls_mensaje + "~nZona"
			ls_colu[li_cont1]	= "zona_codigo"
		End If
		
		If Isnull(dw_1.Object.prod_codigo[ll_fildet]) OR dw_1.Object.prod_codigo[ll_fildet] = 0 Then
			li_cont1 ++
			ls_mensaje 			= ls_mensaje + "~nProductor"
			ls_colu[li_cont1]	= "prod_codigo"
		End If
		
		If Isnull(dw_1.Object.vari_codigo[ll_fildet]) OR dw_1.Object.vari_codigo[ll_fildet] = 0 Then
			li_cont1 ++
			ls_mensaje 			= ls_mensaje + "~nVariedad"
			ls_colu[li_cont1]	= "vari_codigo"
		End If
		/** Correo enviado por VCosta 2012-04-25*/
//		If iuo_Especie.Codigo = 26 Or iuo_Especie.Codigo = 27 Or iuo_Especie.Codigo = 78 Then 
//			If Isnull(dw_1.Object.ctcg_codigo[ll_fildet]) OR dw_1.Object.ctcg_codigo[ll_fildet] = "" Then
//				li_cont1 ++
//				ls_mensaje 			= ls_mensaje + "~nCategoria de Guarda"
//				ls_colu[li_cont1]	= "ctcg_codigo"
//			End If
//		End If

		If dw_2.Object.cced_tipore[1] = 1  and (gstr_parlote.codgen = 0  or IsNull(gstr_parlote.codgen)) Then
			If Isnull(dw_1.Object.lote_codigo[ll_fildet]) OR dw_1.Object.lote_codigo[ll_fildet] = 0 Then
				li_cont1 ++
				ls_mensaje 			= ls_mensaje + "~nLote"
				ls_colu[li_cont1]	= "lote_codigo"
			End If
		ElseIf  gstr_parlote.codgen = 0  or IsNull(gstr_parlote.codgen) Then
			If Isnull(dw_1.Object.orpr_numero[ll_fildet]) OR dw_1.Object.orpr_numero[ll_fildet] = 0 Then
				li_cont1 ++
				ls_mensaje 			= ls_mensaje + "~nNumero Orden Proceso"
				ls_colu[li_cont1]	= "orpr_numero"
			End If	

			If Isnull(dw_1.Object.orpr_tipord[ll_fildet]) OR dw_1.Object.orpr_tipord[ll_fildet] = 0 Then
				li_cont1 ++
				ls_mensaje 			= ls_mensaje + "~nTipo Orden Proceso"
				ls_colu[li_cont1]	= "orpr_tipord"
			End If	
	End If
		 
		If IsNull(dw_1.Object.total_frutos[ll_fildet]) OR dw_1.Object.total_frutos[ll_fildet] <> 100 Then
			li_cont1 ++
			ls_mensaje 			= ls_mensaje + "~nTotal Frutos no es igual a 100 %"
			ls_colu[li_cont1]	= "total_frutos"
		End If
		 
	Next
End If

If li_cont > 0 OR li_cont1 > 0 Then
	MessageBox("Error de Consistencia", "Falta el ingreso de :" + &
	ls_mensaje + ".", StopSign!, Ok!)
	If li_cont1 > 0 Then
		dw_1.SetColumn(ls_colu[1])
		dw_1.SetFocus()
	ElseIf li_cont > 0 Then
		dw_2.SetColumn(ls_colu[1])
		dw_2.SetFocus()
	End If
	Message.DoubleParm = -1
	
Else
	
	li_Cliente		=	dw_2.Object.clie_codigo[1]
	ll_Numero	=	dw_2.Object.cced_numero[1]
	li_Tipo		=	dw_2.Object.cced_tipore[1]
	li_planta		=  dw_2.Object.plde_codigo[1]
	li_lote			=  dw_1.Object.lote_codigo[il_fila]
		
	//Bloqueo para grabación de tablas
	UpDate dbo.parempresa
		 Set	empr_disco	=	:gstr_parempresa.empr_disco
	 Where	empr_rutemp	=	:gstr_parempresa.empr_rutemp
		Using Sqlca;
		
	If Sqlca.SqlCode = -1 Then
		MessageBox('Error', 'No se pudo actualiza tabla ParamEmpresa', StopSign!, OK!)
		Return
	End If
	
		Select IsNull(Max(cced_secuen), 0)
			Into	:li_Secuencia
			From dbo.ctlcalevaludescartedeta 
			Where cced_numero	=	:ll_Numero 
			And	clie_codigo	=	:li_Cliente
			And	cced_tipore = :li_Tipo
			Using Sqlca;
			
			If Sqlca.SqlCode = -1 Then
				MessageBox('Error', 'No se pudo obtener Siguiente correlativo descarte.', StopSign!, OK!)
				Return
			End If
	
		For ll_Fila = 1 TO dw_1.RowCount()	
		
			If dw_1.GetItemStatus(ll_Fila, 0, Primary!) = New! OR &
				dw_1.GetItemStatus(ll_Fila, 0, Primary!) = NewModIfied! Then
				
				li_Secuencia ++
				
				dw_1.SetItem(ll_Fila, "cced_numero", ll_Numero)
				dw_1.SetItem(ll_Fila, "cced_tipore", li_Tipo)
				dw_1.SetItem(ll_Fila, "clie_codigo", li_Cliente)
				dw_1.SetItem(ll_Fila, "lote_pltcod", li_planta)//planta y especie se agrega 05/03/06
				dw_1.SetItem(ll_Fila, "lote_espcod", iuo_Especie.Codigo)
				dw_1.SetItem(ll_Fila, "cced_secuen", li_Secuencia)
				dw_1.Object.cced_totfru[ll_Fila] = dw_1.Object.total_frutos[ll_Fila] 
			End If	
		Next
End If	

HabilitaEncab(FALSE)
end event

event ue_guardar;
//Captura_Usuario()

IF dw_1.AcceptText() = -1 THEN RETURN

SetPointer(HourGlass!)

w_main.SetMicroHelp("Grabando información...")

Message.DoubleParm = 0

TriggerEvent("ue_antesguardar")

IF Message.DoubleParm = -1 THEN RETURN

IF wf_actualiza_db(False) THEN
	w_main.SetMicroHelp("Información Grabada.")
	//pb_eliminar.Enabled	= True
	pb_imprimir.Enabled	= True
ELSE
	w_main.SetMicroHelp("No se puede Grabar información.")
	Message.DoubleParm = -1
	RETURN
END IF
end event

type dw_1 from w_mant_encab_deta_csd`dw_1 within w_maed_ctlcaldescarteespecies
event ue_nomover pbm_syscommand
integer x = 32
integer y = 560
integer width = 4613
integer height = 1192
integer taborder = 80
string title = "Detalle Evaluación Fruta Comercial"
string dataobject = "dw_mues_ctlcaldescarteespecies"
boolean vscrollbar = false
boolean hsplitscroll = true
end type

event type long dw_1::ue_seteafila(unsignedlong wparam, long lparam);il_fila	= This.GetRow()

RETURN 0
end event

event dw_1::rowfocuschanged;ib_datos_ok = True



end event

event dw_1::losefocus;AcceptText()

end event

event dw_1::itemchanged;String	ls_Nula, ls_Columna

SetNull(ls_Nula)
ls_Columna	= dwo.name

Choose Case ls_Columna
	Case 'zona_codigo'
		If Not iuo_Zonas.Existe(Long(Data), True, Sqlca) Then
			This.SetItem(1, ls_Columna, Long(ls_nula))
		  	Return 1
		Else
			This.GetChild('prod_codigo', idwc_productores)
			idwc_productores.SetTransObject(sqlca)
			idwc_productores.Retrieve(iuo_Zonas.Codigo)
		End If
		
	Case "prod_codigo"
		If Not iuo_productor.existe(Long(Data), True, sqlca) Then
			This.SetItem(Row, ls_Columna, Long(ls_nula))
			Return 1
		Else
			If dw_2.Object.cced_tipore[1] = 1 Then
				
			ElseIf dw_2.Object.cced_tipore[1] = 2 Then
				If Not IsNull(iuo_Orden.Productor) Then
					If gstr_parlote.codgen = 0 Then
						If Long(data) <> iuo_Orden.Productor Then
							MessageBox('Error', 'Productor no esta en orden de proceso.Se Deja el de la Orden')
							This.SetItem(Row, ls_Columna, iuo_Orden.Productor)
							Return 1
						End If	
					End If
				End If
			End If
		End If
      
	 Case "vari_codigo"
		If Not iuo_variedades.Existe(iuo_Especie.Codigo,Integer(data),True, sqlca) Then
			This.SetItem(Row, ls_Columna, Integer(ls_nula))
			Return 1
		End If
		
	 Case "lote_codigo" 	
		If NoExisteLotes(Integer(data), 3) Then
			This.Setitem(Row, ls_Columna,Integer(ls_Nula))
			Return 1
		End If
		  
	Case 'orpr_numero'
		If Not iuo_Orden.Existe(iuo_Plantas.Codigo, This.Object.orpr_tipord[Row], Long(data), True, Sqlca, gi_CodExport) Then
			This.Setitem(Row, ls_Columna,Long(ls_Nula))
			Return 1
		Else
			iuo_Productor.Existe(iuo_Orden.Productor, False, sqlca)
			
			This.Object.lote_pltcod[Row]	=	iuo_Orden.Planta
			This.Object.lote_espcod[Row]	=	iuo_Orden.Especie 
			This.Object.prod_codigo[Row]	=	iuo_Orden.Productor
			This.Object.vari_codigo[Row]	=	iuo_Orden.Variedad
			This.Object.zona_codigo[Row]	=	iuo_Productor.Zona
			This.Setcolumn("cced_precal")
			This.SetFocus()			
		End IF
		
	Case 'ctcg_codigo'
		If Not iuo_CateGuarda.Existe(iuo_Especie.Codigo, Data, True, Sqlca) Then
			This.Setitem(Row, ls_Columna, ls_Nula)
			Return 1
		End If
		
	Case 'cced_deform','cced_defor2','cced_defor3','cced_golsol','cced_hercit', 'cced_mandor', 'cced_mancha', 'cced_insect', 'cced_russet', 'cced_residu', &
	'cced_otrcal', 'cced_pudric', 'cced_machuc', 'cced_frublan', 'cced_herabi', 'cced_deshid', 'cced_otrcon', 'cced_fruexp', 'cced_blandos', 'cced_piella', &
	'cced_bajcol', 'cced_ausped','cced_precal', 'cced_hersie', 'cced_medlun', 'cced_partid', 'cced_otrher', 'cced_frudes', &
	'cced_desgar', 'cced_herabi', 'cced_pittin', 'cced_mancon', 'cced_danins', 'cced_carpar', 'cced_oleoce', 'cced_ombras', 'cced_parper', 'cced_peteca', &
	'cced_pateta', 'cced_ausros', 'cced_ombabi', 'cced_pedlar', 'cced_rugoso', 'cced_acosti', 'cced_bufado', 'cced_creasi', 'cced_fumagi', 'cced_danfri', &
	'cced_sobmad', 'cced_grieta', 'cced_rusosc', 'cced_quesol', 'cced_otrins', 'cced_colvir', 'cced_escama', 'cced_defor4'
			If wf_ValidaDatos(Row, ls_Columna, Dec(Data)) = -1 Then
				MessageBox('Atención', 'La sumatoria de defectos debe ser igual a 100.', Exclamation!)
				This.Setitem(Row, ls_Columna, Long(ls_Nula))
			Return 1
			End If
End Choose

pb_grabar.Enabled	=	True
end event

event dw_1::getfocus;//return 0
end event

event dw_1::doubleclicked;//
end event

event dw_1::clicked;IF Row > 0 THEN
	il_fila = Row
	This.SelectRow(0,False)
	This.SetRow(il_fila)
	This.SelectRow(il_fila,False)
END IF

RETURN 0
end event

event dw_1::itemerror;call super::itemerror;Return 1 
end event

event dw_1::buttonclicked;call super::buttonclicked;String	ls_columna

ls_columna = dwo.name

Choose Case ls_columna
	Case "buscalote"
		If iuo_Especie.Codigo = 26 Or iuo_Especie.Codigo = 27 Or iuo_Especie.Codigo = 78 Then
			wf_Buscalote(1)
		Else
			wf_Buscalote(2)
		End If
		
	Case 'b_orden'
		wf_BuscaOrden()
		
End Choose
end event

event dw_1::sqlpreview;//
end event

type dw_2 from w_mant_encab_deta_csd`dw_2 within w_maed_ctlcaldescarteespecies
integer x = 983
integer y = 40
integer width = 2688
integer height = 484
string dataobject = "dw_maed_ctlcaldescarteespecies"
end type

event dw_2::itemchanged;String	ls_Columna, ls_Nula

SetNull(ls_Nula)

ls_Columna	=	dwo.Name

dw_2.AcceptText()

Choose Case ls_Columna
	Case 'clie_codigo'
		If Not iuo_Clientes.Existe(Long(Data), True, Sqlca) Then
			This.SetItem(1, ls_Columna, Integer(ls_nula))
			Return 1
		Else
			istr_mant.argumento[5] = data
		End If
		
	Case "plde_codigo"
		If Not iuo_plantas.existe(Integer(data),TRUE,sqlca) Then
			This.SetItem(1, ls_Columna, Integer(ls_nula))
			Return 1
		Else
			istr_mant.argumento[4] = data
		End If	
		
	Case "plde_codpak"
		If Not iuo_packing.existePacking(Integer(data),TRUE,sqlca) Then
			This.SetItem(1, "plde_codpak", Integer(ls_nula))
			Return 1
		Else
			istr_mant.argumento[3] = data
		End If	
				
	Case "cced_numero"			
		If Existefolio(ls_columna, data, This.Object.cced_tipore[row]) Then
			istr_mant.argumento[1]	=	data	
			This.TriggerEvent("ue_recuperadatos")
		Else
			istr_mant.argumento[1] =	data	
		End If	
			
	Case "ccin_codigo"
      If Not iuo_ctlcalinspectores.existe(sqlca,Integer(data),TRUE) Then
			This.SetItem(1, ls_Columna, Long(ls_nula))
		  	Return 1	
		End If
		
	Case 'cced_tipore'
		If iuo_Especie.Codigo <> 78 And Data = '3' Then
			MessageBox('Atencion', 'Solo puede seleccionar pre proceso para limones.', StopSign!, Ok!)
			This.SetItem(1, ls_Columna, Long(ls_nula))
		  	Return 1	
		Else
			If iuo_Especie.Codigo = 26 Or iuo_Especie.Codigo = 27 Or iuo_Especie.Codigo = 78 Or &
				iuo_Especie.Codigo = 81 Or iuo_Especie.Codigo = 21 Or iuo_Especie.Codigo = 41 Or &
				iuo_Especie.Codigo = 23 Or iuo_Especie.Codigo = 82 Then
				dw_1.SetReDraw(False)
				
				If data = '1' Then
					If iuo_Especie.Codigo = 81 Then 
						dw_1.DataObject = 'dw_mues_ctlcaldescartepaltas_huerto'
					ElseIf iuo_Especie.Codigo = 21 Then 
						dw_1.DataObject = 'dw_mues_ctlcaldescarteespecies_huerto'
					ElseIf iuo_Especie.Codigo = 41 Then 
						dw_1.DataObject = 'dw_mues_ctlcaldescartekiwis_huerto'
					ElseIf iuo_Especie.Codigo = 23 Then
						dw_1.DataObject = 'dw_mues_ctlcaldescarteciruela_huerto'
					ElseIf iuo_Especie.Codigo = 82 Then
						dw_1.DataObject = 'dw_mues_ctlcaldescartegranados_huerto'
					Else
						dw_1.DataObject = 'dw_mues_ctlcaldescartenaranjas_huerto'
					End If
				Else
					If iuo_Especie.Codigo = 81 Then 
						dw_1.DataObject = 'dw_mues_ctlcaldescartepaltas'
					ElseIf iuo_Especie.Codigo = 21 Then 
						dw_1.DataObject = 'dw_mues_ctlcaldescarteespecies'
					ElseIf iuo_Especie.Codigo = 41 Then 
						dw_1.DataObject = 'dw_mues_ctlcaldescartekiwis'
					ElseIf iuo_Especie.Codigo = 23 Then 
						dw_1.DataObject = 'dw_mues_ctlcaldescarteciruela'
					ElseIf iuo_Especie.Codigo = 82 Then 
						dw_1.DataObject = 'dw_mues_ctlcaldescartegranados'
					Else
						dw_1.DataObject = 'dw_mues_ctlcaldescartenaranjas'
					End If
				End If
				
				wf_child()
				dw_1.SetTransObject(sqlca)
	
				If Existefolio(ls_columna, istr_mant.argumento[1], Long(Data)) Then
					This.TriggerEvent("ue_recuperadatos")
				End If	
				
				dw_1.SetReDraw(True)
			End if
		End If

End Choose

HabilitaIngreso(ls_Columna)
end event

type pb_nuevo from w_mant_encab_deta_csd`pb_nuevo within w_maed_ctlcaldescarteespecies
integer x = 4695
integer y = 368
end type

type pb_eliminar from w_mant_encab_deta_csd`pb_eliminar within w_maed_ctlcaldescarteespecies
integer x = 4695
integer y = 548
end type

type pb_grabar from w_mant_encab_deta_csd`pb_grabar within w_maed_ctlcaldescarteespecies
integer x = 4695
integer y = 732
end type

type pb_imprimir from w_mant_encab_deta_csd`pb_imprimir within w_maed_ctlcaldescarteespecies
integer x = 4695
integer y = 908
end type

type pb_salir from w_mant_encab_deta_csd`pb_salir within w_maed_ctlcaldescarteespecies
integer x = 4695
integer y = 1088
end type

type pb_ins_det from w_mant_encab_deta_csd`pb_ins_det within w_maed_ctlcaldescarteespecies
integer x = 4695
integer taborder = 90
end type

type pb_eli_det from w_mant_encab_deta_csd`pb_eli_det within w_maed_ctlcaldescarteespecies
integer x = 4699
integer taborder = 100
end type

type pb_buscar from w_mant_encab_deta_csd`pb_buscar within w_maed_ctlcaldescarteespecies
integer x = 4695
integer y = 188
end type


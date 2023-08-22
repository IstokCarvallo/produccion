$PBExportHeader$w_maed_programa_procesos.srw
forward
global type w_maed_programa_procesos from w_mant_encab_deta_csd
end type
type dw_3 from datawindow within w_maed_programa_procesos
end type
type dw_4 from datawindow within w_maed_programa_procesos
end type
end forward

global type w_maed_programa_procesos from w_mant_encab_deta_csd
integer width = 4768
integer height = 2504
string title = "Programa de Procesos"
string menuname = ""
dw_3 dw_3
dw_4 dw_4
end type
global w_maed_programa_procesos w_maed_programa_procesos

type variables
uo_especie 			 		iuo_especie
uo_variedades 		 		iuo_variedad
uo_grupoespecie    		iuo_grupo
uo_subgrupoespecie 		iuo_subgrupo
uo_plantadesp		 		iuo_dirigido
uo_tipoenvases				iuo_tien
uo_envases					iuo_envases

uo_Categorias				iuo_Categorias
uo_Etiquetas				iuo_Etiquetas
uo_Recibidores				iuo_Recibidor
uo_Cliente					iuo_Cliente

Boolean						ib_Modifica, ib_AutoCommit
DataWindowChild			idwc_especie, idwc_variedad, idwc_planta, idwc_cliente, &
								idwc_categoria, idwc_grupo, idwc_subgrupo, idwc_embalaje, &
								idwc_etiqueta, idwc_recibidor, idwc_calibre, idwc_dirigido, idwc_envase
						
w_mant_deta_programa_proc_cal	iw_mantencion_1

Integer 						ii_especie, ii_grupo, ii_subgrupo
end variables

forward prototypes
public function boolean existeembalaje (string as_embalaje, integer ai_fila)
public function boolean existeprograma (string as_columna, string as_valor)
public function boolean existeprogramacal (integer ai_planta, integer ai_especie, string as_fecha, integer ai_secuen)
public subroutine habilitadetalle (string as_columna)
public subroutine habilitaencab (boolean habilita)
public subroutine habilitaingreso (string as_columna)
public subroutine habilitatipoingreso (integer ai_tipoingreso)
public function boolean revisadetacal (integer ai_secuen)
protected function boolean wf_actualiza_db (boolean borrando)
public subroutine chequeatipoingreso ()
end prototypes

public function boolean existeembalaje (string as_embalaje, integer ai_fila);String   ls_Nombre, ls_envase, ls_codenva
Integer  li_tipoenva, li_codenva, li_CuentaCalibre, li_especie, li_etiqueta, li_cliente
Boolean	lb_Retorno = True

li_Especie = dw_2.Object.espe_codigo[1]
li_Cliente = dw_2.Object.clie_codigo[1]

SELECT	emba_codigo, enva_tipoen, enva_codigo, etiq_codigo
	INTO	:ls_Nombre, :li_tipoenva, :li_codenva, :li_etiqueta
	FROM	dbo.embalajesprod
  WHERE	emba_codigo	=	:as_embalaje
  AND    clie_codigo =  :li_Cliente;

IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura de Tabla de Embalajes")
	
	lb_Retorno	=	False
ELSEIF sqlca.SQLCode=100 THEN
	MessageBox("Atención","Codigo de Embalaje No ha sido generado. Ingrese Otro.")
	lb_Retorno	=	False
END IF

IF lb_retorno THEN
	
	SELECT Count(caen_calibr) Into :li_cuentacalibre
	  FROM dbo.calibresenvase
	 WHERE espe_codigo = :li_especie
	   AND enva_tipoen = :li_tipoenva
		AND enva_codigo = :li_codenva;
	
	IF sqlca.SQLCode = -1 THEN
		F_ErrorBaseDatos(sqlca, "Lectura de Tabla de Calibres Envases")
	
		lb_Retorno	=	False
	ELSEIF sqlca.SQLCode=100 THEN
		MessageBox("Atención","Codigo de Embalaje No Posee Codigos de Calibres Asociados. Ingrese Otro.")
		lb_Retorno	=	False
	ELSE
		IF li_CuentaCalibre=0 or isnull(li_CuentaCalibre) THEN
			MessageBox("Atención","Codigo de Embalaje No Posee Calibres Asociados. Ingrese Otro.")
			lb_Retorno = False
		ELSE
			lb_Retorno = TRUE
		END IF
	END IF

	SELECT	enva_nombre
	  INTO	:ls_envase
	  FROM	dbo.envases
	  WHERE	enva_tipoen	=	:li_tipoenva
	  	AND 	enva_codigo	=	:li_codenva;

	IF sqlca.SQLCode = -1 THEN
		F_ErrorBaseDatos(sqlca, "Lectura de Tabla de Envases")
	
		lb_Retorno	=	False
	ELSEIF sqlca.SQLCode=100 THEN
		MessageBox("Atención","Codigo de Embalaje No Posee Codigos de Envases. Ingrese Otro.")
		lb_Retorno	=	False
	END IF
	
	IF lb_retorno THEN
		dw_1.Object.enva_tipoen[ai_fila]	=	li_tipoenva
		dw_1.Object.enva_codigo[ai_fila]	=	li_codenva
		dw_1.Object.enva_nombre[ai_fila]	=	ls_envase
		istr_Mant.Argumento[7]			=	String(li_TipoEnva)
		istr_Mant.Argumento[8]			=	String(li_CodEnva)
	END IF
	IF gstr_paramplanta.etiquetaembalaje = 1 THEN
		dw_1.Object.etiq_codigo[ai_fila] = li_etiqueta
		dw_1.Object.todasetq[ai_fila] = 0
	END IF
		
END IF

RETURN lb_Retorno
end function

public function boolean existeprograma (string as_columna, string as_valor);Integer	li_Planta, li_Especie, li_Cliente
Long		ll_Numero
Boolean	lb_Retorno = True

dw_2.Accepttext()

li_Planta	=	dw_2.Object.plde_codigo[1]
li_Especie	=	dw_2.Object.espe_codigo[1]
ll_Numero	=	dw_2.Object.ppre_numero[1]
li_Cliente	=	dw_2.Object.clie_codigo[1]

CHOOSE CASE as_columna
	CASE "plde_codigo"
		li_Planta	=	Integer(as_Valor)	
		
	CASE "espe_codigo"
		li_Especie	=	Integer(as_Valor)
			
	CASE "ppre_numero"
		ll_Numero	=	Long(as_Valor)
		
END CHOOSE
li_Planta	=	dw_2.object.plde_codigo[1]


IF Isnull(li_Planta) OR Isnull(li_Especie) OR IsNull(ll_Numero) THEN
	lb_Retorno	=	False
ELSE
	SELECT	ppre_numero
		INTO	:ll_Numero
		FROM	dbo.spro_programaprocenca
		WHERE	plde_codigo	=	:li_Planta
		AND	espe_codigo	=	:li_Especie
		AND	ppre_numero	=	:ll_Numero
		AND   clie_codigo =  :li_Cliente;
		
	IF sqlca.SQLCode = -1 THEN
		F_ErrorBaseDatos(sqlca, "Lectura de Tabla de Programa de Procesos")
		
		lb_Retorno	=	False
	ELSEIF SQLCA.SQLCode = 100 THEN
		lb_Retorno	=	False
	END IF
END IF

RETURN lb_Retorno
end function

public function boolean existeprogramacal (integer ai_planta, integer ai_especie, string as_fecha, integer ai_secuen);Long		ll_Numero
Boolean	lb_Retorno = True
String   ls_Fecha
Date ld_fecha

ls_fecha  = Mid(as_fecha,1,10)
ld_fecha = Date(ls_fecha)

//SELECT	Count(pprd_secuen)
//	INTO	:ll_Numero
//	FROM	dbo.spro_programaproccal
//	WHERE	plde_codigo	=	:ai_Planta
//	AND	espe_codigo	=	:ai_especie
//	AND	ppre_feccre	=	:ld_fecha
//	AND   pprd_secuen	=	:ai_secuen ;
//	
//IF sqlca.SQLCode = -1 THEN
//	F_ErrorBaseDatos(sqlca, "Lectura de Tabla de Programa de Procesos Calibres")
//	
//	lb_Retorno	=	False
//ELSEIF sqlca.SQLCode = 100 THEN
//	lb_Retorno	=	False
//END IF
//
IF ll_numero>0 THEN
	lb_Retorno	=	True
ELSE
	lb_Retorno	=	False
END IF
RETURN lb_Retorno
end function

public subroutine habilitadetalle (string as_columna);Boolean	lb_Estado = True

IF as_Columna <> "cate_codigo" AND &
	(dw_1.Object.cate_codigo[il_fila] = 0 OR IsNull(dw_1.Object.cate_codigo[il_fila])) THEN
	lb_Estado = False
END IF
	
IF as_Columna <> "emba_codigo" AND &
	(dw_1.Object.emba_codigo[il_fila] = "" OR IsNull(dw_1.Object.emba_codigo[il_fila])) THEN
	lb_Estado = False
END IF
end subroutine

public subroutine habilitaencab (boolean habilita);IF Habilita THEN
	dw_2.Object.espe_codigo.Protect				=	0
	dw_2.Object.ppre_numero.Protect				=	0
	dw_2.Object.ppre_feccre.Protect				=	0
	dw_2.Object.clie_codigo.Protect				=	0
	dw_2.Object.espe_codigo.Color				=	0
	dw_2.Object.ppre_numero.Color				=	0
	dw_2.Object.ppre_feccre.Color					=	0
	dw_2.Object.clie_codigo.Color					=	0
	dw_2.Object.espe_codigo.BackGround.Color	=	RGB(255,255,255)
	dw_2.Object.ppre_numero.BackGround.Color	=	RGB(255,255,255)
	dw_2.Object.ppre_feccre.BackGround.Color		=	RGB(255,255,255)
	dw_2.Object.clie_codigo.BackGround.Color		=	RGB(255,255,255)
ELSE
	dw_2.Object.espe_codigo.Protect				=	1
	dw_2.Object.ppre_numero.Protect				=	1
	dw_2.Object.ppre_feccre.Protect				=	1
	dw_2.Object.clie_codigo.Protect				=	1
	dw_2.Object.espe_codigo.Color				=	RGB(255,255,255)
	dw_2.Object.ppre_numero.Color				=	RGB(255,255,255)
	dw_2.Object.ppre_feccre.Color					=	RGB(255,255,255)
	dw_2.Object.clie_codigo.Color					=	RGB(255,255,255)
	dw_2.Object.espe_codigo.BackGround.Color	=	553648127
	dw_2.Object.ppre_numero.BackGround.Color	=	553648127
	dw_2.Object.ppre_feccre.BackGround.Color		=	553648127
	dw_2.Object.clie_codigo.BackGround.Color		=	553648127
END IF
end subroutine

public subroutine habilitaingreso (string as_columna);Integer	li_TipoIngreso
Boolean	lb_Estado = True
Date	ld_Fecha

IF as_Columna <> "ppre_numero" AND &
	(dw_2.Object.ppre_numero[1] = 0 OR IsNull(dw_2.Object.ppre_numero[1])) THEN
	lb_Estado = False
END IF

IF as_Columna <> "ppre_feccre" AND &
	(dw_2.Object.ppre_feccre[1] = ld_Fecha OR IsNull(dw_2.Object.ppre_feccre[1])) THEN
	lb_Estado = False
END IF
	
IF as_Columna <> "espe_codigo" AND &
	(dw_2.Object.espe_codigo[1] = 0 OR IsNull(dw_2.Object.espe_codigo[1])) THEN
	lb_Estado = False
END IF

IF as_Columna <> "ppre_tiping" THEN
	
	li_TipoIngreso	=	dw_2.Object.ppre_tiping[1]
	
	IF as_Columna <> "grva_codigo" AND li_TipoIngreso = 2 AND &
		(dw_2.Object.grva_codigo[1] = 0 OR IsNull(dw_2.Object.grva_codigo[1])) THEN
		lb_Estado = False
	END IF
	
	IF as_Columna <> "grva_codsub" AND li_TipoIngreso = 3 AND &
		(dw_2.Object.grva_codsub[1] = 0 OR IsNull(dw_2.Object.grva_codsub[1])) THEN
		lb_Estado = False
	END IF

	IF as_Columna <> "vari_codigo" AND li_TipoIngreso = 4 AND &
		(dw_2.Object.vari_codigo[1] = 0 OR IsNull(dw_2.Object.vari_codigo[1])) THEN
		lb_Estado = False
	END IF

END IF

dw_1.Enabled	=	lb_Estado
pb_ins_det.Enabled	=	lb_Estado
end subroutine

public subroutine habilitatipoingreso (integer ai_tipoingreso);Integer	li_Null

SetNull(li_Null)

CHOOSE CASE ai_TipoIngreso
	CASE 1 // Por Especie
		dw_2.Object.grva_codigo.BackGround.Color		=	553648127
		dw_2.Object.grva_codsub.BackGround.Color	=	553648127
		dw_2.Object.vari_codigo.BackGround.Color		=	553648127
		dw_2.Object.grva_codigo.Color						=	RGB(255,255,255)
		dw_2.Object.grva_codsub.Color					=	RGB(255,255,255)
		dw_2.Object.vari_codigo.Color						=	RGB(255,255,255)
		
		dw_2.Object.grva_codigo.Protect					=	1
		dw_2.Object.grva_codsub.Protect					=	1
		dw_2.Object.vari_codigo.Protect					=	1
		dw_2.Object.grva_codigo[1]						=	li_Null
		dw_2.Object.grva_codsub[1]						=	li_Null
		dw_2.Object.vari_codigo[1]							=	li_Null

	CASE 2 // Por Grupo
		dw_2.Object.grva_codigo.BackGround.Color		=	RGB(255,255,255)
		dw_2.Object.grva_codsub.BackGround.Color	=	553648127
		dw_2.Object.vari_codigo.BackGround.Color		=	553648127
		dw_2.Object.grva_codigo.Color						=	0
		dw_2.Object.grva_codsub.Color					=	RGB(255,255,255)
		dw_2.Object.vari_codigo.Color						=	RGB(255,255,255)
		dw_2.Object.grva_codigo.Protect					=	0
		dw_2.Object.grva_codsub.Protect					=	1
		dw_2.Object.vari_codigo.Protect					=	1
		dw_2.Object.grva_codsub[1]						=	li_Null
		dw_2.Object.vari_codigo[1]							=	li_Null

	CASE 3 // Por Sub Grupo
		dw_2.Object.grva_codigo.BackGround.Color		=	RGB(255,255,255)
		dw_2.Object.grva_codsub.BackGround.Color	=	RGB(255,255,255)
		dw_2.Object.vari_codigo.BackGround.Color		=	553648127
		dw_2.Object.grva_codigo.Color						=	0
		dw_2.Object.grva_codsub.Color					=	0
		dw_2.Object.vari_codigo.Color						=	RGB(255,255,255)
		dw_2.Object.grva_codigo.Protect					=	0
		dw_2.Object.grva_codsub.Protect					=	0
		dw_2.Object.vari_codigo.Protect					=	1
		dw_2.Object.vari_codigo[1]							=	li_Null

	CASE 4 // Por Variedad
		dw_2.Object.grva_codigo.BackGround.Color		=	553648127
		dw_2.Object.grva_codsub.BackGround.Color	=	553648127
		dw_2.Object.vari_codigo.BackGround.Color		=	RGB(255,255,255)
		dw_2.Object.grva_codigo.Color						=	RGB(255,255,255)
		dw_2.Object.grva_codsub.Color					=	RGB(255,255,255)
		dw_2.Object.vari_codigo.Color						=	0
		
		dw_2.Object.grva_codigo.Protect					=	1
		dw_2.Object.grva_codsub.Protect					=	1
		dw_2.Object.vari_codigo.Protect					=	0
		dw_2.Object.grva_codigo[1]						=	li_Null
		dw_2.Object.grva_codsub[1]						=	li_Null

END CHOOSE
end subroutine

public function boolean revisadetacal (integer ai_secuen);Integer li_row
Boolean lb_Retorno	=	FALSE

li_row = 1
DO WHILE	li_row <= dw_3.RowCount()
	IF dw_3.Object.pprd_secuen[li_row] = ai_secuen THEN
		li_row 		=	dw_3.RowCount()
		lb_Retorno	=	TRUE
	END IF	
	li_row ++
LOOP


RETURN lb_Retorno
end function

protected function boolean wf_actualiza_db (boolean borrando);Boolean	lb_AutoCommit, lb_Retorno

IF Not dw_2.uf_check_required(0) THEN RETURN False

IF Not dw_1.uf_validate(0) THEN RETURN False

lb_AutoCommit		=	sqlca.AutoCommit
sqlca.AutoCommit	=	False

IF Borrando THEN
	IF dw_3.Update(True, False) = 1 THEN
		IF dw_1.Update(True,False) =	1	THEN
			IF dw_2.Update(True, False) = 1 THEN
				Commit;
				
				IF sqlca.SQLCode <> 0 THEN
					F_ErrorBaseDatos(sqlca, This.Title)
				ELSE
					lb_Retorno	=	True
					
					dw_1.ResetUpdate()
					dw_2.ResetUpdate()
					dw_3.ResetUpdate()
				END IF
			ELSE
				F_ErrorBaseDatos(sqlca, This.Title)
				
				RollBack;
			END IF
		ELSE
			F_ErrorBaseDatos(sqlca, This.Title)
			
			RollBack;
		END IF
	ELSE
		F_ErrorBaseDatos(sqlca, This.Title)
		
		RollBack;
	END IF
ELSE
	IF dw_2.Update(True, False) = 1 THEN
		IF dw_1.Update(True,False) =	1	THEN
			IF dw_3.Update(True, False) = 1 THEN
				Commit;
				
				IF sqlca.SQLCode <> 0 THEN
					F_ErrorBaseDatos(sqlca, This.Title)
				ELSE
					lb_Retorno	=	True
					
					dw_3.ResetUpdate()
					dw_1.ResetUpdate()
					dw_2.ResetUpdate()
				END IF
			ELSE
				F_ErrorBaseDatos(sqlca, This.Title)
				
				RollBack;
			END IF
		ELSE
			F_ErrorBaseDatos(sqlca, This.Title)
			
			RollBack;
		END IF
	ELSE
		F_ErrorBaseDatos(sqlca, This.Title)
		
		RollBack;
	END IF
END IF

sqlca.AutoCommit	=	lb_AutoCommit

RETURN lb_Retorno
end function

public subroutine chequeatipoingreso ();IF NOT IsNull(dw_2.Object.vari_codigo[1]) THEN
	dw_2.Object.ppre_tiping[1] = 4
	HabilitaTipoIngreso(4)
	RETURN
ELSEIF dw_2.Object.grva_codsub[1] > 0 THEN
	dw_2.Object.ppre_tiping[1] = 3
	HabilitaTipoIngreso(3)
	RETURN
ELSEIF NOT IsNull(dw_2.Object.grva_codigo[1]) THEN
	dw_2.Object.ppre_tiping[1] = 2
	HabilitaTipoIngreso(2)
	RETURN
ELSE
	dw_2.Object.ppre_tiping[1] = 1
	HabilitaTipoIngreso(1)
	RETURN
END IF
end subroutine

on w_maed_programa_procesos.create
int iCurrent
call super::create
this.dw_3=create dw_3
this.dw_4=create dw_4
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.dw_3
this.Control[iCurrent+2]=this.dw_4
end on

on w_maed_programa_procesos.destroy
call super::destroy
destroy(this.dw_3)
destroy(this.dw_4)
end on

event ue_nuevo;Long		ll_modif

ib_ok	= True

istr_mant.Solo_Consulta	=	istr_mant.UsuarioSoloConsulta

IF Not istr_mant.Solo_Consulta THEN
	CHOOSE CASE wf_modifica()
		CASE -1
			ib_ok = False
		CASE 0
			ll_modif	=	dw_1.GetNextModified(0, Primary!)
			ll_modif	+=	dw_3.GetNextModified(0, Primary!)
					
			IF dw_1.RowCount() > 0 and ll_modif > 0 THEN
				CHOOSE CASE MessageBox("Grabar registro(s)","Desea Grabar la información ?", Question!, YesNoCancel!)
					CASE 1
						Message.DoubleParm = 0
						This.TriggerEvent("ue_guardar")
						IF message.DoubleParm = -1 THEN ib_ok = False
					CASE 3
						ib_ok	= False
						RETURN
				END CHOOSE
			END IF
	END CHOOSE
END IF

IF Not ib_ok THEN RETURN

dw_1.Reset()
dw_3.Reset()

dw_3.GetChild("pprc_calini", idwc_calibre)
idwc_calibre.SetTransObject(sqlca)
idwc_calibre.Retrieve(-1,-1,-1)

pb_eli_det.Enabled		=	False
pb_ins_det.Enabled		=	False
pb_grabar.Enabled		=	False
pb_eliminar.Enabled		=	False
pb_imprimir.Enabled		=	False
dw_2.Enabled				=	True

dw_2.SetRedraw(False)
dw_2.Reset()
dw_2.InsertRow(0)
dw_2.SetRedraw(True)

dw_2.SetFocus()

dw_2.Object.plde_codigo[1]		=	gstr_ParamPlanta.CodigoPlanta
dw_2.Object.clie_codigo[1]		=	gi_CodExport
dw_2.Object.plde_coorde[1]	=	gstr_ParamPlanta.CodigoPlanta
dw_2.Object.ppre_feccre[1]		=	Date(Today())

istr_Mant.Argumento[1]	=	String(gstr_ParamPlanta.CodigoPlanta)
istr_Mant.Argumento[3]  =  String(Date(Today()))

HabilitaTipoIngreso(1)
HabilitaEncab(True)
end event

event open;/* 
Argumentos
istr_Mant.Argumento[1]	=	Código Planta
istr_Mant.Argumento[2]	=	Especie
istr_Mant.Argumento[3]	=	Numero Programa
istr_Mant.Argumento[4]	=	Categoria
istr_Mant.Argumento[5]	=	Etiqueta
istr_Mant.Argumento[6]	=	Embalaje
istr_Mant.Argumento[7]	=	Tipo envase
istr_Mant.Argumento[8]	=	Envase
istr_Mant.Argumento[9]	=	Tipo Selección
istr_Mant.Argumento[10]	=	Cantidad de Cajas
istr_Mant.Argumento[11]	=	Código de Recibidor
istr_Mant.Argumento[12] =  Secuencia Detalle
istr_mant.argumento[13] =	Codigo Cliente
*/
x				= 0
y				= 0
This.Height	= 2500
im_menu		= m_principal

This.ParentWindow().ToolBarVisible	= True
im_menu.Item[1].Item[6].Enabled		= True
im_menu.Item[7].Visible					= True

iuo_especie				=	Create uo_especie
iuo_variedad			=	Create uo_variedades
iuo_grupo				=	Create uo_grupoespecie
iuo_subgrupo			=	Create uo_subgrupoespecie
iuo_dirigido         		=  Create uo_plantadesp
iuo_tien					=	Create uo_tipoenvases
iuo_envases				=	Create uo_envases
iuo_Categorias			=	Create uo_Categorias
iuo_Etiquetas			=	Create uo_Etiquetas
iuo_Recibidor			=	Create uo_Recibidores
iuo_Cliente				=	Create uo_Cliente

istr_Mant.Argumento[1]	=	String(gstr_ParamPlanta.CodigoPlanta)
istr_Mant.Argumento[2]	=	''
istr_Mant.Argumento[3]  =  ''
istr_mant.Argumento[13] =	String(gi_codexport)
dw_3.SetTransObject(sqlca)

dw_1.SetTransObject(sqlca)

dw_2.GetChild("plde_codigo", idwc_planta)
idwc_planta.SetTransObject(sqlca)

IF idwc_planta.Retrieve() = 0 THEN
	idwc_planta.InsertRow(0)
END IF

dw_2.GetChild("plde_coorde", idwc_dirigido)
idwc_dirigido.SetTransObject(sqlca)

IF idwc_dirigido.Retrieve() = 0 THEN
	idwc_dirigido.InsertRow(0)
END IF

//Especie
dw_2.GetChild("espe_codigo", idwc_especie)
idwc_especie.SetTransObject(sqlca)
IF idwc_especie.Retrieve() = 0 THEN
	MessageBox("Atención","Falta Registrar Especies")
ELSE
	idwc_especie.InsertRow(0)
END IF

//Cliente
dw_2.GetChild("clie_codigo", idwc_cliente)
idwc_cliente.SetTransObject(sqlca)
idwc_cliente.Retrieve()
idwc_cliente.InsertRow(0)

//Grupo
dw_2.GetChild("grva_codigo",idwc_grupo)
idwc_grupo.SetTransObject(Sqlca)
idwc_grupo.InsertRow(0)

//Sub Grupo
dw_2.GetChild("grva_codsub",idwc_subgrupo)
idwc_subgrupo.SetTransObject(Sqlca)
idwc_subgrupo.InsertRow(0)

//Variedad
dw_2.GetChild("vari_codigo",idwc_variedad)
idwc_variedad.SetTransObject(Sqlca)
idwc_variedad.InsertRow(0)

dw_1.GetChild("emba_codigo", idwc_embalaje)
idwc_embalaje.SetTransObject(sqlca)
idwc_embalaje.Retrieve(gi_codexport)

dw_1.GetChild("cate_codigo", idwc_categoria)
idwc_categoria.SetTransObject(sqlca)
idwc_categoria.Retrieve()
		
dw_1.Getchild("reci_codigo", idwc_recibidor)
idwc_recibidor.SetTransObject(sqlca)
idwc_recibidor.Retrieve()
			
dw_1.Getchild("etiq_codigo", idwc_etiqueta)
idwc_etiqueta.SetTransObject(sqlca)
idwc_etiqueta.Retrieve()

dw_3.GetChild("pprc_calini", idwc_calibre)
idwc_calibre.SetTransObject(sqlca)
idwc_calibre.InsertRow(0)

dw_1.Getchild("enva_codigo", idwc_envase)
idwc_envase.SetTransObject(sqlca)
idwc_envase.Retrieve(0)

dw_2.SetTransObject(sqlca)

istr_Mant.dw				=	dw_3
istr_Mant.solo_consulta =	False

pb_nuevo.PostEvent(Clicked!)

GrabaAccesoAplicacion(True, id_FechaAcceso, it_HoraAcceso, This.Title, "Acceso a Aplicación", 1)
end event

event ue_recuperadatos;call super::ue_recuperadatos;Long		ll_fila_d, ll_fila_e, respuesta,i

DO
	ll_fila_e	=	dw_2.Retrieve(Integer(istr_Mant.Argumento[1]), &
										  Integer(istr_Mant.Argumento[2]), &
										  Long(istr_Mant.Argumento[3]),Integer(istr_mant.Argumento[13]))
	IF ll_fila_e = -1 THEN
		respuesta = MessageBox(	"Error en Base de Datos", "No es posible conectar la Base de Datos.", &
						Information!, RetryCancel!)		
	ELSE
		ChequeaTipoIngreso()
		DO
			IF dw_1.Retrieve(Integer(istr_Mant.Argumento[1]),&
							     Integer(istr_Mant.Argumento[2]),&
							     Long(istr_Mant.Argumento[3]),Integer(istr_mant.Argumento[13])) = -1 THEN
				respuesta = MessageBox("Error en Base de Datos", "No es posible conectar la Base de Datos.", &
												Information!, RetryCancel!)
			END IF
			for i = 1 to dw_1.rowcount()
				if isnull(dw_1.object.reci_codigo[i]) then
					dw_1.object.todosrec[i] = 1
				end if			
				if isnull(dw_1.object.etiq_codigo[i]) then
					dw_1.object.todasetq[i] = 1
				end if			
			next
			IF dw_3.Retrieve(Integer(istr_Mant.Argumento[1]), &
								  Integer(istr_Mant.Argumento[2]), &
								  Long(istr_Mant.Argumento[3]),Integer(istr_mant.Argumento[13])) = -1 THEN     
				respuesta = MessageBox("Error en Base de Datos", "No es posible conectar la Base de Datos.", &
												Information!, RetryCancel!)
			ELSE
				pb_eliminar.Enabled	=	True
				pb_grabar.Enabled		=	True
				pb_imprimir.Enabled	=	True
				pb_ins_det.Enabled	=	True
				pb_eli_det.Enabled	=	True
				HabilitaEncab(False)
				dw_1.SetFocus()
			END IF
		LOOP WHILE respuesta = 1

		IF respuesta = 2 THEN Close(This)
	END IF
	dw_2.SetRedraw(True)
LOOP WHILE respuesta = 1

IF respuesta = 2 THEN Close(This)
end event

event ue_modifica_detalle();call super::ue_modifica_detalle;IF dw_1.RowCount() > 0 THEN

	istr_mant.agrega	= False
	istr_mant.borra	= False
	
	istr_Mant.Argumento[7]	=	String(dw_1.Object.enva_tipoen[il_fila])
	istr_Mant.Argumento[8]	= 	String(dw_1.Object.enva_codigo[il_fila])
	istr_Mant.Argumento[9] 	=	String(dw_1.Object.pprd_tipsel[il_fila])
	istr_Mant.Argumento[10] =	String(dw_1.Object.pprd_cancaj[il_fila])
	istr_Mant.Argumento[11] =	String(dw_1.Object.reci_codigo[il_fila])
	istr_Mant.Argumento[12] =	String(dw_1.Object.pprd_secuen[il_fila])

	istr_Mant.dw	=	dw_1
	istr_Mant.dw2	=	dw_3

	OpenWithParm(iw_mantencion_1, istr_Mant)

	dw_3.SetFilter("")
	dw_3.Filter()

END IF
end event

event ue_borra_detalle();call super::ue_borra_detalle;Integer	li_borra

SetPointer(HourGlass!)

ib_borrar = True
w_main.SetMicroHelp("Validando la eliminación de detalle...")

IF RevisaDetaCal(il_fila) THEN
	Message.DoubleParm = 0

	This.TriggerEvent ("ue_validaborrar_detalle")

	IF Message.DoubleParm = -1 THEN RETURN

	istr_Mant.Borra	= True
	istr_Mant.Agrega	= False

	istr_Mant.Argumento[9] 	= String(dw_1.Object.pprd_tipsel[il_fila])
	istr_Mant.Argumento[10] = String(dw_1.Object.pprd_cancaj[il_fila])
	istr_Mant.Argumento[11] = String(dw_1.Object.reci_codigo[il_fila])
	istr_Mant.Argumento[12] = String(dw_1.Object.pprd_secuen[il_fila])

	istr_Mant.dw	=	dw_1
	istr_Mant.dw2	=	dw_3

	OpenWithParm(iw_mantencion_1, istr_Mant)

	istr_Mant = Message.PowerObjectParm

	IF istr_Mant.Respuesta = 1 THEN
		li_borra	=	dw_1.DeleteRow(0)
		IF li_Borra = 1 THEN
			li_Borra = dw_3.RowsMove(1,dw_3.RowCount(),Primary!,dw_3,1,Delete!)
		END IF
		 
		IF li_borra = 1 THEN
			dw_3.SetFilter("")
			dw_3.Filter()
		
			ib_borrar = False
			w_main.SetMicroHelp("Borrando Registro...")
			SetPointer(Arrow!)
		ELSE
			ib_borrar = False
			MessageBox(This.Title,"No se puede borrar actual registro.")
		END IF

		IF dw_1.RowCount() = 0 THEN pb_eli_det.Enabled = False
	END IF

	istr_Mant.borra	 = False
ELSE
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
END IF
end event

event ue_borrar();IF dw_2.RowCount() < 1 THEN RETURN

SetPointer(HourGlass!)

ib_borrar = True
w_main.SetMicroHelp("Validando la eliminación...")

Message.DoubleParm = 0

This.TriggerEvent ("ue_validaborrar")

IF Message.DoubleParm = -1 THEN RETURN

IF dw_1.RowCount() > 0 THEN dw_1.RowsMove(1,dw_1.RowCount(),Primary!,dw_1,1,Delete!)
IF dw_3.RowCount() > 0 THEN dw_3.RowsMove(1,dw_3.RowCount(),Primary!,dw_3,1,Delete!)

IF dw_2.DeleteRow(0) = 1 THEN
		ib_borrar = False
		w_main.SetMicroHelp("Borrando Registro...")
		
		ib_AutoCommit		=	SQLCA.AutoCommit
		SQLCA.AutoCommit	=	False
		
		IF wf_actualiza_db(True) THEN
			w_main.SetMicroHelp("Registro Borrado...")
			This.TriggerEvent("ue_nuevo")
			SetPointer(Arrow!)
		ELSE
			w_main.SetMicroHelp("Registro no Borrado...")
		END IF			
ELSE
	ib_borrar = False
	MessageBox(This.Title,"No se puede borrar actual registro.")
END IF
end event

event ue_seleccion;call super::ue_seleccion;Str_Busqueda	lstr_busq

lstr_busq.argum[1] = ""
lstr_busq.argum[2] = ""
lstr_busq.argum[3] = ""

lstr_busq.argum[1]	=	String(dw_2.Object.plde_codigo[1])
lstr_busq.argum[2]	=	istr_mant.argumento[2]
lstr_busq.argum[3]	=	string(dw_2.object.clie_codigo[1])

OpenWithParm(w_busc_spro_programaprocenca, lstr_busq)
lstr_busq	= Message.PowerObjectParm

//*****************************************
IF lstr_busq.argum[1] = "close" then Return
//*****************************************

IF lstr_busq.argum[3] <> "" THEN
	istr_mant.argumento[1] = lstr_busq.argum[1]
	istr_mant.argumento[2] = lstr_busq.argum[2]
	istr_mant.argumento[3] = lstr_busq.argum[3]
	
	dw_2.GetChild("grva_codigo",idwc_grupo)
	idwc_grupo.SetTransObject(Sqlca)
	
	IF idwc_grupo.Retrieve(Integer(istr_mant.argumento[2])) = 0 THEN
		idwc_grupo.InsertRow(0)
	END IF
	
	IF lstr_busq.argum[4]<>"" or IsNull(lstr_busq.argum[4]) THEN
		dw_2.GetChild("grva_codsub",idwc_subgrupo)
		idwc_subgrupo.SetTransObject(Sqlca)
		IF idwc_subgrupo.Retrieve(Integer(istr_mant.argumento[2]),Integer(lstr_busq.argum[5])) = 0 THEN
			idwc_subgrupo.InsertRow(0)
		END IF
	END IF	
	
	TriggerEvent("ue_recuperadatos")
ELSE
	pb_buscar.SetFocus()
END IF
end event

event ue_nuevo_detalle;call super::ue_nuevo_detalle;dw_1.SetColumn("reci_codigo")

IF il_fila > 0 THEN
	pb_eliminar.Enabled	= 	True
	pb_grabar.Enabled		= 	True
END IF

istr_Mant.Borra	=	False
istr_Mant.Agrega	=	True

il_fila = dw_1.GetRow()

IF dw_1.RowCount() > 0 THEN
   IF RevisaDetaCal(il_fila) THEN
		il_fila = dw_1.InsertRow(0)
		
		dw_1.Object.pprd_secuen[il_Fila]	=	dw_1.Object.pprd_secuen[il_Fila - 1] + 1
		
		dw_1.Object.todosrec[il_fila] = 1
		dw_1.Object.todasetq[il_fila] = 1
		
		dw_1.ScrollToRow(il_fila)
		dw_1.SetRow(il_fila)
		dw_1.SetFocus()
		dw_1.SetColumn(1)
		
	ELSE
		MessageBox("Atención",	"Debe Ingresar un Detalle para la Secuencia " + String(il_fila,'000') + &
		            					   	" antes de Insertar otro registro.")
   END IF								
ELSE
	il_fila = dw_1.InsertRow(0)	
	
	dw_1.Object.pprd_secuen[il_Fila]	=	il_Fila
	
	dw_1.Object.todosrec[il_fila] = 1
	dw_1.Object.todasetq[il_fila] = 1
	
	dw_1.ScrollToRow(il_fila)
	dw_1.SetRow(il_fila)
	dw_1.SetFocus()
	dw_1.SetColumn(1)
	
END IF	

IF il_fila > 1 THEN
	pb_eli_det.Enabled	=	True
END IF	
	
IF dw_1.RowCount() > 0 THEN
	pb_eliminar.Enabled	=	True
	pb_grabar.Enabled		=	True
END IF
end event

event ue_antesguardar;call super::ue_antesguardar;Long			ll_Fila, ll_Numero
Integer		li_Planta, li_Especie, li_Cliente

li_Planta	=	dw_2.Object.plde_codigo[1]
li_Especie	=	dw_2.Object.espe_codigo[1]
ll_Numero	=	dw_2.Object.ppre_numero[1]
li_Cliente	=	dw_2.Object.clie_codigo[1]

FOR ll_Fila = 1 TO dw_1.RowCount()
	
	IF dw_1.GetItemStatus(ll_Fila, 0, Primary!) = NewModified! THEN
		dw_1.Object.plde_codigo[ll_Fila]	=	li_Planta
		dw_1.Object.espe_codigo[ll_Fila]	=	li_Especie
		dw_1.Object.ppre_numero[ll_Fila]	=	ll_Numero
		dw_1.Object.clie_codigo[ll_Fila]	=	li_Cliente
	END IF
NEXT

FOR ll_Fila = 1 TO dw_3.RowCount()
	
	IF dw_3.GetItemStatus(ll_Fila, 0, Primary!) = NewModified! THEN
		dw_3.Object.plde_codigo[ll_Fila]	=	li_Planta
		dw_3.Object.espe_codigo[ll_Fila]	=	li_Especie
		dw_3.Object.ppre_numero[ll_Fila]	=	ll_Numero
		dw_3.Object.clie_codigo[ll_Fila]	=	li_Cliente		
	END IF
NEXT

IF dw_1.deletedCount() > 0 THEN
	wf_actualiza_db(TRUE)
	Message.DoubleParm = -1
	RETURN
END IF	

end event

event ue_imprimir;SetPointer(HourGlass!)

Long		fila

istr_info.titulo	= "PROGRAMA DE PROCESO"
istr_info.copias	= 1

OpenWithParm(vinf,istr_info)
vinf.dw_1.DataObject = "dw_info_programaproceso"
vinf.dw_1.SetTransObject(sqlca)
fila = vinf.dw_1.Retrieve(Integer(istr_Mant.Argumento[13]), &
								  Integer(istr_Mant.Argumento[1]), &
								  Integer(istr_Mant.Argumento[2]), &
								  Long(istr_Mant.Argumento[3]))

IF fila = -1 THEN
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
					"de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
ELSEIF fila = 0 THEN
	MessageBox( "No Existe información", "No existe información para este informe.", StopSign!, Ok!)
ELSE
	F_Membrete(vinf.dw_1)
	vinf.dw_1.Modify('DataWindow.Print.Preview = Yes')
	vinf.dw_1.Modify('DataWindow.Print.Preview.Zoom = 75')
	vinf.Visible	= True
	vinf.Enabled	= True
END IF

SetPointer(Arrow!)
end event

type dw_1 from w_mant_encab_deta_csd`dw_1 within w_maed_programa_procesos
integer x = 14
integer y = 1000
integer width = 4329
integer height = 1368
integer taborder = 10
string title = "Detalle del Programa de Proceso"
string dataobject = "dw_mant_prograproceso_deta"
end type

event dw_1::doubleclicked;//
end event

event dw_1::itemchanged;Integer	li_Null
String	ls_Columna, ls_Nula

ls_Columna = GetColumnName()
SetNull(li_Null)
SetNull(ls_Nula)

CHOOSE CASE ls_Columna
	CASE "todosrec"	
		IF Data = "1" THEN
 			This.SetItem(il_fila, "reci_codigo", li_Null)
		END IF

	CASE "todasetq"	
		IF Data = "1" THEN
 			This.SetItem(il_fila, "etiq_codigo", li_Null)
		END IF
		
	CASE "emba_codigo"
		IF NOT existeembalaje(data,il_fila) THEN
			This.SetItem(il_fila,"emba_codigo", ls_Nula)
			This.SetFocus()
			RETURN 1
		ELSE
			istr_Mant.Argumento[6]	=	Data
		END IF
		
	CASE "enva_codigo"
		IF Integer(This.Object.enva_tipoen[row]) < 1 THEN 
			MessageBox("Error", "Seleccione primero el tipo de envase")
			Return
		END IF
		IF NOT iuo_envases.Existe(Integer(This.Object.enva_tipoen[row]), Integer(Data), True, Sqlca) THEN
			This.Object.enva_codigo[Row]	=	li_Null
			This.Object.enva_nombre[Row]	=	String(li_Null)
			RETURN 1
		ELSE
			This.Object.enva_nombre[Row]	=	iuo_envases.Nombre	
		END IF
		
	CASE "cate_codigo"
		IF NOT iuo_Categorias.Existe(Integer(data), True, Sqlca) THEN
			This.SetItem(il_fila,"cate_codigo", li_Null)
			This.SetFocus()
			RETURN 1
		ELSE
			istr_Mant.Argumento[4]	=	Data
		END IF
		
	CASE "reci_codigo"
		IF Isnull(data) OR data = "" THEN
			istr_Mant.Argumento[11]	=	Data
			This.Object.todosrec[il_fila] = 1
		ELSEIF NOT iuo_Recibidor.Existe(Long(data), True, Sqlca) THEN
			This.SetItem(il_fila,"reci_codigo", li_Null)
			This.SetFocus()
			RETURN 1
		ELSE
			istr_Mant.Argumento[11]	=	Data
			This.Object.todosrec[il_fila] = 0
		END IF

	CASE "etiq_codigo"
		IF Isnull(data) OR data = "" THEN
			istr_Mant.Argumento[5]	=	Data
			This.Object.todasetq[il_fila] = 1
		ELSEIF NOT iuo_Etiquetas.Existe(Integer(data), True, Sqlca) THEN
			This.SetItem(il_fila,"etiq_codigo", li_Null)
			This.SetFocus()
			RETURN 1
		ELSE
			istr_Mant.Argumento[5]	=	Data
			This.Object.todasetq[il_fila] = 0
		END IF

	CASE "enva_tipoen"
		IF NOT iuo_tien.Existe(Integer(data), True, sqlca) THEN
			This.Object.enva_tipoen[row]	=	li_null
			Return 1
		ELSE
			This.object.enva_tipoen[row]	=	li_null
			This.object.enva_nombre[row]	=	String(li_null)
		END IF

END CHOOSE

Habilitadetalle(ls_columna)
end event

event dw_1::buttonclicking;call super::buttonclicking;Integer			li_Contador
String			ls_Mensaje, ls_Columna[]
str_busqueda	lstr_busq

CHOOSE CASE dwo.Name
	CASE "b_detalle"
		If Row > 0 Then
			il_fila = Row
		End If

		If IsNull(this.Object.enva_tipoen[il_fila]) OR this.Object.enva_tipoen[il_fila] = 0 Then
		  	li_Contador ++
			ls_Mensaje 					+=	"~nTipo Envase"
			ls_Columna[li_Contador]	=	"enva_tipoen"
	   End If	

		If IsNull(this.Object.enva_codigo[il_fila]) OR this.Object.enva_codigo[il_fila] = 0 Then
		  	li_Contador ++
			ls_Mensaje 					+=	"~nEnvase"
			ls_Columna[li_Contador]	=	"enva_codigo"
	   End If	
		
		If IsNull(this.Object.cate_codigo[il_fila]) OR this.Object.cate_codigo[il_fila] = 0 Then
		  	li_Contador ++
			ls_Mensaje 					+=	"~nCategoria"
			ls_Columna[li_Contador]	=	"cate_codigo"
	   End If
      
		If this.Object.todosrec[il_fila] = 0 Then
			If IsNull(this.Object.reci_codigo[il_fila]) OR this.Object.reci_codigo[il_fila] = 0 Then
			  	li_Contador ++
				ls_Mensaje 					+=	"~nRecibidor"
				ls_Columna[li_Contador]	=	"reci_codigo"
	   	End If
		End If
		
		If this.Object.todasetq[il_fila] = 0 Then
			If IsNull(this.Object.etiq_codigo[il_fila]) OR this.Object.etiq_codigo[il_fila] = 0 Then
			  	li_Contador ++
				ls_Mensaje 					+=	"~nEtiquetas"
				ls_Columna[li_Contador]	=	"etiq_codigo"
	   	End If
		End If

		If li_Contador > 0 Then
			MessageBox("Error de Consistencia", "Falta el ingreso de :" + ls_Mensaje + ".", StopSign!, Ok!)
			dw_1.SetColumn(ls_Columna[1])
			dw_1.SetFocus()
		Else	
			Parent.TriggerEvent("ue_modIfica_detalle")
	   End If
		
	CASE "b_envase"
		If Integer(This.Object.enva_tipoen[row]) < 1 Then 
			MessageBox("Error", "Seleccione primero el tipo de envase")
			Return
		End If
		
		lstr_busq.Argum[1]	=	String(This.Object.enva_tipoen[row])
		OpenWithParm(w_busc_envases, lstr_busq)
		lstr_busq	=	Message.PowerObjectParm
		
		If UpperBound(lstr_busq.Argum) > 3 Then
			This.Object.enva_codigo[row]	=	Integer(lstr_busq.Argum[2])
			This.Object.enva_nombre[row]	=	lstr_busq.Argum[3]
		End If
End CHOOSE

end event

event dw_1::getfocus;//


end event

event dw_1::rowfocuschanged;//
end event

event dw_1::clicked;IF Row > 0 THEN
	il_fila = Row
END IF

RETURN 0
end event

event dw_1::dwnkey;//
end event

type dw_2 from w_mant_encab_deta_csd`dw_2 within w_maed_programa_procesos
integer x = 795
integer y = 40
integer width = 2697
integer height = 880
integer taborder = 90
string dataobject = "dw_mant_prograproceso_enca"
end type

event dw_2::itemchanged;Integer	li_Null
String	ls_Columna, ls_Nula

ls_Columna = dwo.Name

SetNull(li_Null)
SetNull(ls_Nula)

CHOOSE Case ls_Columna
	Case "clie_codigo"
		If Not iuo_Cliente.Existe(Integer(data), True, Sqlca) Then
			This.SetItem(row,ls_Columna,Integer(ls_Nula))
			Return 1
		End If
		istr_mant.Argumento[13] = data		
		
	Case "espe_codigo"	
		If NOT iuo_Especie.Existe(Integer(Data),True,SqlCa) Then  
			ii_especie	= integer(ls_Nula)
			istr_mant.argumento[2] = ""
			This.SetItem(row,"espe_codigo", integer(ls_Nula))
			This.SetItem(row,'grva_codigo', Integer(ls_Nula))
			This.SetItem(row,'grva_codsub', Integer(ls_Nula))
			This.SetItem(row,'vari_codigo', Integer(ls_Nula))			
			This.SetFocus()
			Return 1
	   Else
			ii_especie = Integer(Data)
			istr_mant.argumento[2] = Data
						
			this.SetItem(row,'grva_codigo', Integer(ls_Nula))  
			dw_2.GetChild("grva_codigo",idwc_grupo)
			idwc_grupo.SetTransObject(Sqlca)
			idwc_grupo.Retrieve(ii_Especie)
			/**/
	   		this.SetItem(row,'grva_codsub', Integer(ls_Nula))
			dw_2.GetChild("grva_codsub",idwc_subgrupo)
			idwc_subgrupo.SetTransObject(Sqlca)
			idwc_subgrupo.Retrieve(ii_Especie,0)
			/**/
			this.SetItem(row,'vari_codigo', Integer(ls_Nula))
			dw_2.GetChild("vari_codigo",idwc_variedad)
			idwc_variedad.SetTransObject(Sqlca)
			idwc_variedad.Retrieve(ii_Especie)
			
			If existeprograma(ls_columna, Data) Then
				Parent.TriggerEvent("ue_recuperadatos")
			End If	
		End If	
	
	Case "ppre_numero"
		istr_mant.argumento[3] = Data
		If Existeprograma(ls_columna, Data) Then
			Parent.TriggerEvent("ue_recuperadatos")
		End If	

	Case 	"ppre_tiping"
		HabilitaTipoIngreso(Integer(data))

	Case "grva_codigo"
		
		If NOT iuo_Grupo.Existe(Integer(istr_mant.Argumento[2]),Integer(data),True,SqlCa) Then
			ii_grupo = integer(ls_nula)
			This.SetItem(il_fila, "grva_codigo", Integer(ls_Nula))
			This.SetFocus()
			Return 1
		Else
			ii_grupo	=	Integer(Data)
			this.SetItem(il_fila,'grva_codsub', Integer(ls_Nula))
			this.SetItem(il_fila,'vari_codigo', Integer(ls_Nula))
			dw_2.GetChild("grva_codsub",idwc_subgrupo)
			idwc_subgrupo.SetTransObject(Sqlca)
			idwc_subgrupo.Retrieve(ii_Especie,ii_Grupo)
		
			idwc_variedad.SetFilter("grva_codigo=" + String(ii_grupo))
			idwc_variedad.Filter()
		End If	
		
	Case "grva_codsub"
		If NOT iuo_SubGrupo.Existe(Integer(istr_mant.Argumento[2]),ii_Grupo,Integer(data),True,SqlCa) Then
			This.SetItem(il_fila, "grva_codsub", integer(ls_Nula))
			ii_subgrupo	= integer(ls_Nula)
			This.SetFocus()
			Return 1
		Else	
			ii_subgrupo	=	Integer(Data)
			this.SetItem(il_fila,'vari_codigo', Integer(ls_Nula))
			idwc_variedad.SetFilter("grva_codigo=" + String(ii_grupo) + &
		                        " And grva_codsub=" + String(ii_subgrupo))
			idwc_variedad.Filter()
		End If
		
	Case "vari_codigo"
		If NOT iuo_variedad.Existe(Integer(istr_mant.Argumento[2]),Integer(data),TRUE,SQLCA) Then
			This.SetItem(il_fila, "vari_codigo", integer(ls_Nula))
			This.SetFocus()
			Return 1
		End If	
	
End CHOOSE

HabilitaIngreso(ls_Columna)
end event

type pb_nuevo from w_mant_encab_deta_csd`pb_nuevo within w_maed_programa_procesos
integer x = 4416
integer y = 348
integer taborder = 40
end type

type pb_eliminar from w_mant_encab_deta_csd`pb_eliminar within w_maed_programa_procesos
integer x = 4416
integer y = 528
integer taborder = 50
end type

type pb_grabar from w_mant_encab_deta_csd`pb_grabar within w_maed_programa_procesos
integer x = 4416
integer y = 712
integer taborder = 60
end type

type pb_imprimir from w_mant_encab_deta_csd`pb_imprimir within w_maed_programa_procesos
integer x = 4416
integer y = 888
integer taborder = 70
end type

type pb_salir from w_mant_encab_deta_csd`pb_salir within w_maed_programa_procesos
integer x = 4416
integer y = 1068
integer taborder = 80
end type

type pb_ins_det from w_mant_encab_deta_csd`pb_ins_det within w_maed_programa_procesos
integer x = 4416
integer y = 1456
integer taborder = 20
end type

type pb_eli_det from w_mant_encab_deta_csd`pb_eli_det within w_maed_programa_procesos
integer x = 4416
integer y = 1628
integer taborder = 100
end type

type pb_buscar from w_mant_encab_deta_csd`pb_buscar within w_maed_programa_procesos
integer x = 4416
integer y = 168
integer taborder = 30
end type

type dw_3 from datawindow within w_maed_programa_procesos
boolean visible = false
integer x = 466
integer y = 1784
integer width = 2107
integer height = 516
integer taborder = 110
boolean bringtotop = true
boolean titlebar = true
string dataobject = "dw_mant_prograproceso_cal"
boolean vscrollbar = true
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

type dw_4 from datawindow within w_maed_programa_procesos
boolean visible = false
integer x = 398
integer y = 180
integer width = 192
integer height = 156
integer taborder = 50
boolean bringtotop = true
string title = "none"
string dataobject = "dw_info_programaproceso"
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type


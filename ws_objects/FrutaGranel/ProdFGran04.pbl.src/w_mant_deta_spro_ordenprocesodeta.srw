$PBExportHeader$w_mant_deta_spro_ordenprocesodeta.srw
$PBExportComments$Mantención Detalle de Ordenes de Proceso
forward
global type w_mant_deta_spro_ordenprocesodeta from w_mant_detalle_csd
end type
end forward

global type w_mant_deta_spro_ordenprocesodeta from w_mant_detalle_csd
integer width = 2665
integer height = 1628
string title = "DETALLE DE PROGRAMA DE PROCESO CALIBRES"
end type
global w_mant_deta_spro_ordenprocesodeta w_mant_deta_spro_ordenprocesodeta

type variables

DataWindowChild		idwc_especiedeta, idwc_categoriadeta, idwc_variedaddeta, idwc_grupodeta, &
							idwc_subgrupodeta, idwc_embalajedeta
							
Integer  ii_especie, ii_grupo, ii_subgrupo

uo_especie 				iuo_especie
uo_variedades 			iuo_variedad
uo_categorias     	iuo_categoria
uo_grupoespecie   	iuo_grupo
uo_subgrupoespecie	iuo_subgrupo
end variables

forward prototypes
public function boolean duplicado (string as_columna, string as_valor)
public function boolean existeembalaje (string as_embalaje)
public subroutine buscarecibidor ()
public function boolean existerecibidor (long al_recibidor)
end prototypes

public function boolean duplicado (string as_columna, string as_valor);Long		ll_Fila
String	ls_TipoEnvase, ls_CodEnvase, ls_CondEnvase, ls_Calidad

//ls_TipoEnvase	=	String(dw_1.Object.enva_tipoen[il_Fila])
//ls_CodEnvase	=	String(dw_1.Object.enva_codigo[il_Fila])
//ls_CondEnvase	=	String(dw_1.Object.fgme_conenv[il_Fila])
//ls_Calidad		=	String(dw_1.Object.cale_calida[il_Fila])
//
//CHOOSE CASE as_Columna
//	CASE "enva_tipoen"
//		ls_TipoEnvase	=	as_Valor
//
//	CASE "enva_codigo"
//		ls_CodEnvase	=	as_Valor
//
//	CASE "fgme_conenv"
//		ls_CondEnvase	=	as_Valor
//
//	CASE "cale_calida"
//		ls_Calidad		=	as_Valor
//
//END CHOOSE
//
//ll_Fila	=	dw_1.Find("enva_tipoen = " + ls_TipoEnvase + " AND " + &
//							"enva_codigo = " + ls_CodEnvase + " AND " + &
//							"fgme_conenv = " + ls_CondEnvase + " AND " + &
//							"cale_calida = '" + ls_Calidad + "'", &
//							1, dw_1.RowCount())
//
//IF ll_Fila > 0 AND ll_Fila <> il_Fila THEN
//	MessageBox("Error", "Registro ya fue ingresada anteriormente", Information!, Ok!)
//	RETURN True
//ELSE
	RETURN False
//END IF
end function

public function boolean existeembalaje (string as_embalaje);
String   ls_Nombre
Boolean	lb_Retorno = True

SELECT	emba_codigo
	INTO	:ls_Nombre
	FROM	dba.embalajes
  WHERE	emba_codigo	=	:as_embalaje;
	
IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura de Tabla de Embalajes")
	
	lb_Retorno	=	False
ELSEIF sqlca.SQLCode=100 THEN
	MessageBox("Atención","Codigo de Embalaje No ha sido generado. Ingrese Otro.")
	lb_Retorno	=	False
END IF

RETURN lb_Retorno
end function

public subroutine buscarecibidor ();Str_busqueda	lstr_busq

OpenWithParm(w_busc_recibidores, lstr_busq)

lstr_busq	= Message.PowerObjectParm

IF lstr_busq.argum[1] = "" THEN
	dw_1.SetColumn("reci_codigo")
	dw_1.Object.reci_nombre[il_fila]	=	'Todos'
	dw_1.Object.todos[il_fila]			= 	"1"
	dw_1.SetFocus()
ELSE
	dw_1.Object.reci_codigo[il_fila]	=	Long(lstr_busq.argum[1])
	dw_1.Object.reci_nombre[il_fila]	=	lstr_busq.argum[2]
	dw_1.Object.todos[il_fila] 		= 	"0"

	dw_1.SetFocus()
END IF

RETURN
end subroutine

public function boolean existerecibidor (long al_recibidor);Long		ll_Numero
String   ls_Nombre
Boolean	lb_Retorno = True

SELECT	reci_codigo, reci_nombre
	INTO	:ll_Numero, :ls_Nombre
	FROM	dba.recibidores
  WHERE	reci_codigo	=	:al_recibidor;
	
IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura de Tabla de Recibidores")
	
	lb_Retorno	=	False
ELSEIF sqlca.SQLCode=100 THEN
	MessageBox("Atención","Codigo de Recibidor No ha sido generado. Ingrese Otro.")
	lb_Retorno	=	False
END IF

dw_1.Object.reci_nombre[1]=ls_nombre
RETURN lb_Retorno
end function

on w_mant_deta_spro_ordenprocesodeta.create
call super::create
end on

on w_mant_deta_spro_ordenprocesodeta.destroy
call super::destroy
end on

event ue_recuperadatos();call super::ue_recuperadatos;Integer li_Null

SetNull(li_Null)

ias_campo[1]	=	String(dw_1.object.reci_codigo[il_fila])
ias_campo[2]	=	String(dw_1.object.espe_codigo[il_fila])
ias_campo[3]	=	String(dw_1.object.grva_codigo[il_fila])
ias_campo[4]	=	String(dw_1.object.grva_codsub[il_fila])
ias_campo[5]	=	String(dw_1.object.vari_codigo[il_fila])
ias_campo[6]	=	String(dw_1.object.cate_codigo[il_fila])
ias_campo[7]	=	String(dw_1.object.emba_codigo[il_fila])
ias_campo[8]	=	String(dw_1.object.orpd_canbul[il_fila])
ias_campo[9]	=	String(dw_1.object.orpd_bulpro[il_fila])

IF IsNull(ias_campo[1]) or ias_campo[1]	=	"" THEN
   dw_1.SetItem(il_fila,'reci_codigo', li_Null)
	dw_1.Object.todos[il_fila]				 		= 	"1"
	dw_1.Object.reci_nombre[il_fila] 			= 	"Todos"
	dw_1.Object.reci_codigo.Protect				=	1
	dw_1.Object.reci_codigo.BackGround.Color	=	RGB(192,192,192)

END IF

IF istr_mant.Agrega THEN

	dw_1.Object.plde_codigo[il_Fila]				=	Integer(istr_Mant.Argumento[1])
	dw_1.Object.orpr_tipord[il_Fila]				=	Integer(istr_Mant.Argumento[2])
	dw_1.Object.orpr_numero[il_Fila]				=	Integer(istr_Mant.Argumento[3])
	dw_1.Object.espe_codigo[il_Fila]				=	Integer(istr_Mant.Argumento[5])
	dw_1.Object.todos[il_fila]						= 	"1"
	dw_1.Object.reci_nombre[il_fila] 			= 	"Todos"
	dw_1.Object.reci_codigo.Protect				=	1
	dw_1.Object.reci_codigo.BackGround.Color	=	RGB(192,192,192)
	dw_1.SetItem(il_fila,'reci_codigo', li_Null)
		
END IF

end event

event ue_deshace;call super::ue_deshace;IF UpperBound(ias_campo) > 0 THEN
	IF IsNull(ias_campo[1]) = False THEN
	   dw_1.object.reci_codigo[il_fila] = Long(ias_campo[1])
	END IF
	dw_1.object.espe_codigo[il_fila] = integer(ias_campo[2])
	dw_1.object.grva_codigo[il_fila] = integer(ias_campo[3])
	dw_1.object.grva_codsub[il_fila] = integer(ias_campo[4])
	dw_1.object.vari_codigo[il_fila] = integer(ias_campo[5])
	dw_1.object.cate_codigo[il_fila] = integer(ias_campo[6])
	dw_1.object.emba_codigo[il_fila] = ias_campo[7]	
	dw_1.object.orpd_canbul[il_fila] = long(ias_campo[8])
	dw_1.object.orpd_bulpro[il_fila] = long(ias_campo[9])
END IF
end event

event ue_antesguardar();Integer	li_Contador
String	ls_Mensaje, ls_Columna[]

IF IsNull(dw_1.Object.emba_codigo[il_Fila]) OR &
	dw_1.Object.emba_codigo[il_Fila] = "" THEN
	li_Contador ++
	ls_Mensaje 					+=	"~nCodigo de Embalaje"
	ls_Columna[li_Contador]	=	"emba_codigo"
END IF

IF IsNull(dw_1.Object.cate_codigo[il_Fila]) OR &
	dw_1.Object.cate_codigo[il_Fila] = 0 THEN
	li_Contador ++
	ls_Mensaje 					+=	"~nCodigo de Categoria"
	ls_Columna[li_Contador]	=	"cate_codigo"
END IF

IF IsNull(dw_1.Object.espe_codigo[il_Fila]) OR &
	dw_1.Object.espe_codigo[il_Fila] = 0 THEN
	li_Contador ++
	ls_Mensaje 					+=	"~nCodigo de Especie"
	ls_Columna[li_Contador]	=	"espe_codigo"
END IF

IF IsNull(dw_1.Object.vari_codigo[il_Fila]) OR &
	dw_1.Object.vari_codigo[il_Fila] = 0 THEN
	li_Contador ++
	ls_Mensaje 					+=	"~nCódigo de Variedad"
	ls_Columna[li_Contador]	=	"vari_codigo"
END IF

IF IsNull(dw_1.Object.orpd_bulpro[il_Fila]) OR &
	dw_1.Object.orpd_bulpro[il_Fila] = 0 THEN
	li_Contador ++
	ls_Mensaje 					+=	"~nBultos Procesados"
	ls_Columna[li_Contador]	=	"orpd_bulpro"
END IF

IF IsNull(dw_1.Object.orpd_canbul[il_Fila]) OR &
	dw_1.Object.orpd_canbul[il_Fila] = 0 THEN
	li_Contador ++
	ls_Mensaje 					+=	"~nCantidad de Bultos"
	ls_Columna[li_Contador]	=	"orpd_canbul"
END IF

IF li_Contador > 0 THEN
	MessageBox("Error de Consistencia", "Falta el ingreso de :" + ls_Mensaje + ".", StopSign!, Ok!)
	dw_1.SetColumn(ls_Columna[1])
	dw_1.SetFocus()
	Message.DoubleParm = -1
END IF
end event

event ue_nuevo();call super::ue_nuevo;Integer li_Null

SetNull(li_Null)

dw_1.Object.plde_codigo[il_Fila]	=	Integer(istr_Mant.Argumento[1])
dw_1.Object.orpr_tipord[il_Fila]	=	Integer(istr_Mant.Argumento[2])
dw_1.Object.orpr_numero[il_Fila]	=	Integer(istr_Mant.Argumento[3])
dw_1.Object.espe_codigo[il_Fila]	=	Integer(istr_Mant.Argumento[5])

dw_1.Object.todos[il_fila] 		= 	"1"
dw_1.Object.reci_nombre[il_fila]	=	'Todos'

dw_1.SetItem(il_fila,'reci_codigo', li_Null)
dw_1.Object.reci_codigo.Protect	=	1
dw_1.Object.reci_codigo.BackGround.Color	=	RGB(192,192,192)
end event

event open;/* 
	Argumentos
		istr_Mant.Argumento[1]	=	Código Planta
		istr_Mant.Argumento[2]	=	Tipo de Orden
		istr_Mant.Argumento[3]	=	Número de Orden
		istr_Mant.Argumento[4]	=	
*/

x	= 100
y	= 450

This.Icon	=	Gstr_apl.Icono

iuo_especie				=	Create uo_especie
iuo_variedad			=	Create uo_variedades
iuo_categoria        =  Create uo_categorias
iuo_grupo				=	Create uo_grupoespecie
iuo_subgrupo         =  Create uo_subgrupoespecie

PostEvent("ue_recuperadatos")

istr_mant = Message.PowerObjectParm

dw_1.GetChild("espe_codigo", idwc_especiedeta)
idwc_especiedeta.SetTransObject(sqlca)
IF idwc_especiedeta.Retrieve() = 0 THEN
	idwc_especiedeta.InsertRow(0)
END IF

dw_1.GetChild("emba_codigo", idwc_embalajedeta)
idwc_embalajedeta.SetTransObject(sqlca)
IF idwc_embalajedeta.Retrieve() = 0 THEN
	idwc_embalajedeta.InsertRow(0)
END IF

dw_1.GetChild("cate_codigo", idwc_categoriadeta)
idwc_categoriadeta.SetTransObject(sqlca)
IF idwc_categoriadeta.Retrieve() = 0 THEN
	idwc_categoriadeta.InsertRow(0)
END IF

dw_1.GetChild("grva_codigo", idwc_grupodeta)
idwc_grupodeta.SetTransObject(sqlca)
IF idwc_grupodeta.Retrieve(integer(istr_mant.argumento[5])) = 0 THEN
	idwc_grupodeta.InsertRow(0)
END IF

dw_1.GetChild("grva_codsub", idwc_subgrupodeta)
idwc_subgrupodeta.SetTransObject(sqlca)
IF idwc_subgrupodeta.Retrieve(integer(istr_mant.argumento[5]),0) = 0 THEN
	idwc_subgrupodeta.InsertRow(0)
END IF

dw_1.GetChild("vari_codigo", idwc_variedaddeta)
idwc_variedaddeta.SetTransObject(sqlca)
IF idwc_variedaddeta.Retrieve(integer(istr_mant.argumento[5])) = 0 THEN
	idwc_variedaddeta.InsertRow(0)
END IF


dw_1.SetTransObject(sqlca)
istr_mant.dw.ShareData(dw_1)



end event

type pb_ultimo from w_mant_detalle_csd`pb_ultimo within w_mant_deta_spro_ordenprocesodeta
boolean visible = false
end type

type pb_siguiente from w_mant_detalle_csd`pb_siguiente within w_mant_deta_spro_ordenprocesodeta
boolean visible = false
end type

type pb_anterior from w_mant_detalle_csd`pb_anterior within w_mant_deta_spro_ordenprocesodeta
boolean visible = false
end type

type pb_primero from w_mant_detalle_csd`pb_primero within w_mant_deta_spro_ordenprocesodeta
boolean visible = false
end type

type pb_cancela from w_mant_detalle_csd`pb_cancela within w_mant_deta_spro_ordenprocesodeta
integer x = 2418
integer y = 408
end type

type pb_acepta from w_mant_detalle_csd`pb_acepta within w_mant_deta_spro_ordenprocesodeta
integer x = 2418
integer y = 232
end type

type pb_salir from w_mant_detalle_csd`pb_salir within w_mant_deta_spro_ordenprocesodeta
integer x = 2418
integer y = 580
end type

type dw_1 from w_mant_detalle_csd`dw_1 within w_mant_deta_spro_ordenprocesodeta
integer x = 0
integer y = 0
integer width = 2299
integer height = 1544
string dataobject = "dw_mant_spro_ordenprocesodeta"
end type

event dw_1::itemchanged;call super::itemchanged;String  ls_Columna, ls_Nula
Long    ll_Fila

SetNull(ls_Nula)

ls_Columna = dwo.Name

CHOOSE CASE ls_Columna
	CASE "todos"
		IF Data="1" THEN
		   this.SetItem(il_fila,'reci_codigo',Long(ls_Nula))
			this.SetItem(il_fila,'reci_nombre','Todos')
			dw_1.Object.reci_codigo.Protect				=	1
			dw_1.Object.reci_codigo.BackGround.Color	=	RGB(192,192,192)
		ELSE
		   BuscaRecibidor()
			dw_1.Object.reci_codigo.Protect				=	0
			dw_1.Object.reci_codigo.BackGround.Color	=	RGB(255,255,255)
		END IF
	
	CASE "reci_codigo"
		IF NOT existerecibidor(Long(data)) THEN
			This.SetItem(il_fila,"reci_codigo", Long(ls_Nula))
			this.SetItem(il_fila,"reci_nombre", ls_Nula)
			This.SetFocus()
			RETURN 1
		END IF
	
	CASE "emba_codigo"
		IF NOT existeembalaje(data) THEN
			This.SetItem(il_fila,"emba_codigo", ls_Nula)
			This.SetFocus()
			RETURN 1
		END IF

	
	CASE "cate_codigo"
		IF NOT iuo_Categoria.Existe(Integer(data),True,SqlCa) THEN
			This.SetItem(il_fila,"cate_codigo", integer(ls_Nula))
			This.SetFocus()
			RETURN 1
		END IF
	
	CASE "espe_codigo"	
     	 
		IF NOT iuo_Especie.Existe(Integer(Data),True,SqlCa) THEN  
			ii_especie	= integer(ls_Nula)
			This.SetItem(il_fila,"espe_codigo", integer(ls_Nula))
			this.SetItem(il_fila,'grva_codigo', Integer(ls_Nula))
			this.SetItem(il_fila,'grva_codsub', Integer(ls_Nula))
			this.SetItem(il_fila,'vari_codigo', Integer(ls_Nula))			
			This.SetFocus()
			RETURN 1
	   ELSE
			ii_especie = Integer(Data)
			this.SetItem(il_fila,'grva_codigo', Integer(ls_Nula))  
			dw_1.GetChild("grva_codigo",idwc_grupodeta)
			idwc_grupodeta.SetTransObject(Sqlca)
			idwc_grupodeta.Retrieve(ii_Especie)
			/**/
	   	this.SetItem(il_fila,'grva_codsub', Integer(ls_Nula))
			dw_1.GetChild("grva_codsub",idwc_subgrupodeta)
			idwc_subgrupodeta.SetTransObject(Sqlca)
			idwc_subgrupodeta.Retrieve(ii_Especie,0)
			/**/
			this.SetItem(il_fila,'vari_codigo', Integer(ls_Nula))
			dw_1.GetChild("vari_codigo",idwc_variedaddeta)
			idwc_variedaddeta.SetTransObject(Sqlca)
			idwc_variedaddeta.Retrieve(ii_Especie)
		END IF	
	
	CASE "grva_codigo"
		
		IF NOT iuo_Grupo.Existe(ii_Especie,Integer(data),True,SqlCa) THEN
			ii_grupo = integer(ls_nula)
			This.SetItem(il_fila, "grva_codigo", Integer(ls_Nula))
			This.SetFocus()
			RETURN 1
		ELSE
			ii_grupo	=	Integer(Data)
			this.SetItem(il_fila,'grva_codsub', Integer(ls_Nula))
			this.SetItem(il_fila,'vari_codigo', Integer(ls_Nula))
			dw_1.GetChild("grva_codsub",idwc_subgrupodeta)
			idwc_subgrupodeta.SetTransObject(Sqlca)
			idwc_subgrupodeta.Retrieve(ii_Especie,ii_Grupo)
		
			idwc_variedaddeta.SetFilter("grva_codigo=" + String(ii_grupo))
			idwc_variedaddeta.Filter()
		END IF	
		
	CASE "grva_codsub"
		IF NOT iuo_SubGrupo.Existe(ii_Especie,ii_Grupo,Integer(data),True,SqlCa) THEN
			This.SetItem(il_fila, "grva_codsub", integer(ls_Nula))
			ii_subgrupo	= integer(ls_Nula)
			This.SetFocus()
			RETURN 1
		ELSE	
			ii_subgrupo	=	Integer(Data)
			this.SetItem(il_fila,'vari_codigo', Integer(ls_Nula))
			idwc_variedaddeta.SetFilter("grva_codigo=" + String(ii_grupo) + &
		                        " And grva_codsub=" + String(ii_subgrupo))
			idwc_variedaddeta.Filter()
		END IF	
	
	CASE "vari_codigo"
		IF NOT iuo_Variedad.Existe(ii_especie,Integer(data),True,SqlCa) THEN
			This.SetItem(il_fila,"vari_codigo", integer(ls_Nula))
			This.SetFocus()
			RETURN 1
		END IF
		
END CHOOSE
end event

event dw_1::buttonclicked;call super::buttonclicked;CHOOSE CASE dwo.Name
	CASE "b_buscarecibidor"
		BuscaRecibidor()
END CHOOSE
end event

